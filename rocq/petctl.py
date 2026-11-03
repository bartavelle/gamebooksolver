#!/usr/bin/env python3
"""Keep one `pet` (Petanque) process alive and drive it from one-shot commands.

  petctl.py serve --root DIR [--socket PATH]   run the daemon (foreground)
  petctl.py start FILE THEOREM                 open a theorem, print its goals
  petctl.py run ST 'TACTIC.'                   run from state ST, print new goals
  petctl.py script ST 'TAC1.' 'TAC2.' ...      run in sequence, stop at first error
  petctl.py goals ST                           print the goals of state ST
  petctl.py at FILE LINE COL                   state at a 0-based position
  petctl.py root FILE                          state at the start of FILE
  petctl.py toc FILE                           list the document's named items
  petctl.py rpc METHOD JSON                    send any petanque/* request

Vernacular works through `run` too ('Search foo.', 'Print bar.', 'Check baz.');
its output is shown as feedback. States stay valid until the daemon restarts, so
backtracking is just running from an earlier state number.

A request over --timeout seconds kills and restarts pet, invalidating all states.
"""
import argparse
import json
import os
import select
import signal
import socket
import subprocess
import sys
import time

DEFAULT_SOCKET = os.path.join(os.path.dirname(os.path.abspath(__file__)), "pet.sock")
DEFAULT_TIMEOUT = 300.0


class Pet:
    def __init__(self, root, log):
        self.root, self.log = root, log
        self.next_id = 0
        self.spawn()

    def spawn(self):
        self.proc = subprocess.Popen(
            ["pet", f"--root={self.root}"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=self.log, cwd=self.root,
        )
        self.buf = b""

    def kill(self):
        self.proc.kill()
        self.proc.wait()

    def read_message(self, deadline):
        while True:
            head, sep, rest = self.buf.partition(b"\r\n\r\n")
            if sep:
                length = next(
                    int(line.split(b":")[1])
                    for line in head.split(b"\r\n")
                    if line.lower().startswith(b"content-length")
                )
                if len(rest) >= length:
                    self.buf = rest[length:]
                    return json.loads(rest[:length])
            left = deadline - time.monotonic()
            if left <= 0:
                raise TimeoutError
            ready, _, _ = select.select([self.proc.stdout], [], [], left)
            if ready:
                chunk = os.read(self.proc.stdout.fileno(), 1 << 16)
                if not chunk:
                    raise EOFError
                self.buf += chunk

    def request(self, method, params, timeout):
        self.next_id += 1
        body = json.dumps(
            {"jsonrpc": "2.0", "id": self.next_id, "method": method, "params": params}
        ).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
        self.proc.stdin.flush()
        deadline = time.monotonic() + timeout
        messages = []
        while True:
            msg = self.read_message(deadline)
            if msg.get("id") == self.next_id:
                msg["log"] = messages
                return msg
            if msg.get("method") == "window/logMessage":
                messages.append(msg["params"]["message"])


def serve(args):
    root = os.path.abspath(args.root)
    if os.path.exists(args.socket):
        os.unlink(args.socket)
    pet = Pet(root, open(args.socket + ".log", "ab"))
    server = socket.socket(socket.AF_UNIX)
    server.bind(args.socket)
    os.chmod(args.socket, 0o600)
    server.listen()
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))
    print(f"pet serving {root} on {args.socket}", flush=True)
    try:
        while True:
            conn, _ = server.accept()
            with conn:
                req = json.loads(conn.makefile().readline())
                try:
                    res = pet.request(req["method"], req["params"], req["timeout"])
                except (TimeoutError, EOFError, BrokenPipeError) as exc:
                    if not isinstance(exc, TimeoutError):
                        print(f"pet died: {exc!r}", flush=True)
                    pet.kill()
                    pet.spawn()
                    res = {"restarted": type(exc).__name__}
                conn.sendall(json.dumps(res).encode())
    finally:
        pet.kill()
        os.unlink(args.socket)


def call(args, method, params):
    with socket.socket(socket.AF_UNIX) as conn:
        conn.connect(args.socket)
        conn.sendall(json.dumps(
            {"method": method, "params": params, "timeout": args.timeout}
        ).encode() + b"\n")
        data = conn.makefile().read()
    res = json.loads(data)
    if "restarted" in res:
        sys.exit(f"pet restarted after {res['restarted']}: every state number is now invalid")
    return res


def show_goals(args, st):
    res = call(args, "petanque/goals", {"st": st})
    if "error" in res:
        sys.exit(f"ERROR: {res['error'].get('message')}")
    goals = res.get("result")
    if not goals:
        print("(no proof open)")
        return
    focused = goals["goals"]
    stacked = sum(len(left) + len(right) for left, right in goals.get("stack", []))
    print(f"{len(focused)} focused goal(s), {stacked} on the stack, "
          f"{len(goals.get('shelf', []))} shelved, {len(goals.get('given_up', []))} given up")
    if goals.get("bullet"):
        print(f"bullet: {goals['bullet']}")
    for i, goal in enumerate(focused):
        print(f"--- goal {i + 1}" + ("" if i else " (active)"))
        if i == 0:
            for hyp in goal["hyps"]:
                value = f" := {hyp['def']}" if hyp.get("def") else ""
                print(f"{', '.join(hyp['names'])}{value} : {hyp['ty']}")
            print("=" * 30)
        print(goal["ty"])


def show_result(args, res):
    """Print a Run_result; return its state number, or None on error."""
    if "error" in res:
        print(f"ERROR: {res['error'].get('message')}")
        for line in res.get("log", []):
            print(f"log: {line}")
        return None
    result = res["result"]
    print(f"state {result['st']}" + (" (proof finished)" if result["proof_finished"] else ""))
    for _level, message in result.get("feedback", []):
        print(message)
    return result["st"]


def file_uri(path):
    return "file://" + os.path.abspath(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT)
    sub = parser.add_subparsers(dest="cmd", required=True)
    sub.add_parser("serve").add_argument("--root", required=True)
    for name, params in {
        "start": ["file", "thm"], "run": ["st", "tac"], "goals": ["st"],
        "at": ["file", "line", "col"], "root": ["file"], "toc": ["file"],
        "rpc": ["method", "json"],
    }.items():
        cmd = sub.add_parser(name)
        for param in params:
            cmd.add_argument(param, type=int if param in ("st", "line", "col") else str)
    script = sub.add_parser("script")
    script.add_argument("st", type=int)
    script.add_argument("tacs", nargs="+")
    args = parser.parse_args()

    if args.cmd == "serve":
        serve(args)
    elif args.cmd == "start":
        st = show_result(args, call(args, "petanque/start", {"uri": file_uri(args.file), "thm": args.thm}))
        if st is None:
            sys.exit(1)
        show_goals(args, st)
    elif args.cmd == "run":
        st = show_result(args, call(args, "petanque/run", {"st": args.st, "tac": args.tac}))
        if st is None:
            sys.exit(1)
        show_goals(args, st)
    elif args.cmd == "script":
        st = args.st
        for tac in args.tacs:
            print(f">>> {tac}")
            st = show_result(args, call(args, "petanque/run", {"st": st, "tac": tac}))
            if st is None:
                sys.exit(1)
        show_goals(args, st)
    elif args.cmd == "goals":
        show_goals(args, args.st)
    elif args.cmd == "at":
        position = {"line": args.line, "character": args.col}
        st = show_result(args, call(args, "petanque/get_state_at_pos", {"uri": file_uri(args.file), "position": position}))
        if st is None:
            sys.exit(1)
        show_goals(args, st)
    elif args.cmd == "root":
        if show_result(args, call(args, "petanque/get_root_state", {"uri": file_uri(args.file)})) is None:
            sys.exit(1)
    elif args.cmd == "toc":
        res = call(args, "petanque/toc", {"uri": file_uri(args.file)})
        if "error" in res:
            sys.exit(f"ERROR: {res['error'].get('message')}")
        for name, _info in res["result"]:
            print(name)
    elif args.cmd == "rpc":
        print(json.dumps(call(args, args.method, json.loads(args.json)), indent=2))


if __name__ == "__main__":
    main()
