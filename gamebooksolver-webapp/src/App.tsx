import React from 'react';
import './App.css';
import { PROPERTY_TYPES } from '@babel/types';
import * as fzstd from 'fzstd';
import { WState, WCharacterVariable, WNS } from 'gamebooksolver-wasm';

type Chapter = {
  title: string,
  desc: string,
  pchoice: Array<any>,
};

type Item = string | any;

type ScoreDesc = {
  score: number,
  desc: Desc,
  states: number,
};

type Desc = {
  _ccst: {
    _bookid: string,
    _combatSkill: number,
    _discipline: Array<string>,
    _maxendurance: number
  },
  _cvar: {
    _cvflags: Array<string>,
    _cvgold: number,
    _cvitems: Array<[number, Item]>,
  }
}

type PScoreDesc = {
  path: string,
  desc: Desc,
  score: number,
  states: number,
};

function percent(score: number): string {
  if (!score) {
    return "unk"
  }
  return (score * 100).toFixed(2) + "%"
}

type BookInfo = {
  chapters: Array<[number, Chapter]>,
  content: { [key: string]: ScoreDesc },
};

type Books = { [key: string]: BookInfo };

type State = {
  curbook: string | null,
  books: Books,
  bookids: Array<string>,
  sorted_descs: Array<PScoreDesc>,
  chapters: { [key: number]: Chapter },
  cursolpath: string | null,
  cursoldesc: ScoreDesc | null,
  stt: WState | null,
  curchapter: number,
  curcvar: WCharacterVariable | null
};

type StateDesc = {
  score: number,
  states: number,
  chapter: number,
  curhp: number,
  maxhp: number,
  items: Array<[string, number]>,
  flags: Array<string>
  disciplines: Array<string>,
};

function statedesc_show(d: StateDesc) {
  return <table className="table">
    <thead>
      <tr>
        <th scope="col">Score</th>
        <th scope="col">Chapter</th>
        <th scope="col">HP</th>
        <th scope="col">Items</th>
        <th scope="col">Flags</th>
        <th scope="col">Disciplines</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th scope="row">{percent(d.score)}</th>
        <td >{d.chapter}</td>
        <td>{d.curhp + '/' + d.maxhp}</td>
        <td>
          <ul>
            {d.items.map(([i, q]) => <li key={i}>{i} x {q}</li>)}
          </ul>
        </td>
        <td>
          <ul>
            {d.flags.map((f) => <li key={f}>{f}</li>)}
          </ul>
        </td>
        <td>
          <ul>
            {d.disciplines.map((d) => <li key={d}>{d}</li>)}
          </ul>
        </td>
      </tr>
    </tbody>
  </table>
}

function urlpart(bookid: string) {
  switch (bookid) {
    case "Book01": return "01fftd";
    case "Book02": return "02fotw";
    case "Book03": return "03tcok";
    case "Book04": return "04tcod";
    case "Book05": return "05sots";
    default:
      console.log("Unknown bookid: " + bookid);
      return "000"
  }
}


function cvar_diff(curcvar: WCharacterVariable, newcvar: WCharacterVariable): string {
  var changes = [];
  if (curcvar.endurance != newcvar.endurance) {
    changes.push((newcvar.endurance - curcvar.endurance) + "hp");
  }

  function diff(curl: Array<[any, number]>, newl: Array<[any, number]>): Array<string> {
    var out: Array<string> = [];

    function toobject(l: Array<[any, number]>): { [key: string]: number } {
      function tostring(x: any): string {
        if (typeof x === 'string') {
          return x
        } else {
          return JSON.stringify(x)
        }
      }

      var oo: { [key: string]: number } = {};
      for (var i in l) {
        const [ix, q] = l[i];
        oo[tostring(ix)] = q;
      }
      return oo;
    }

    var curo = toobject(curl);
    var newo = toobject(newl);

    const lost = (item: string, quantity: number) => {
      if (quantity > 1) {
        return "-" + quantity + item
      } else {
        return "-" + item
      }
    };
    const won = (item: string, quantity: number) => {
      if (quantity > 1) {
        return "+" + quantity + item
      } else {
        return "+" + item
      }
    };
    const change = (item: string, quantity: number) => {
      if (quantity > 0) {
        return won(item, quantity);
      } else {
        return lost(item, -quantity);
      }
    };

    for (const k in curo) {
      if (!newo[k]) {
        out.push(lost(k, curo[k]));
      } else {
        const q = newo[k] - curo[k];
        if (q != 0) {
          out.push(change(k, q));
        }
      }
    }

    for (const k in newo) {
      if (!curo[k]) {
        out.push(won(k, curo[k]));
      }
    }

    return out;
  }

  function mflags(flgs: Array<any>): Array<[any, number]> {
    return flgs.map((f) => [f, 1])
  }

  changes = changes.concat(diff(curcvar.items, newcvar.items), diff(mflags(curcvar.flags), mflags(newcvar.flags)));

  return changes.join(" ");
}


class App extends React.Component<{}, State> {

  constructor(props: {}) {
    super(props)
    this.state = {
      curbook: null,
      books: {},
      bookids: [],
      sorted_descs: [],
      chapters: {},
      cursolpath: null,
      cursoldesc: null,
      curchapter: 1,
      curcvar: null,
      stt: null
    }
  }

  async componentDidMount() {
    const books: Books = await (await fetch('/books.json')).json();
    const bookids = Object.keys(books).sort();
    const wasmm = await import("gamebooksolver-wasm");

    // don't know why this is required :(
    await wasmm.default();
    wasmm.init_hooks();

    this.setState({
      bookids: bookids,
      books: books,
    });

  }

  async set_soldesc(pth: string) {
    this.setState({
      cursolpath: pth,
      cursoldesc: this.state.books[this.state.curbook!].content[pth]
    })
  }

  async init_solution(pth: string) {
    if (this.state.curcvar) {
      this.state.curcvar.free();
    }
    if (this.state.stt) {
      this.state.stt.free();
    }
    const stt = WState.from_desc(
      JSON.stringify(this.state.books[this.state.curbook!].content[pth].desc),
      JSON.stringify(this.state.books[this.state.curbook!].chapters));
    const cvar = stt.ini_cvar();
    this.setState({
      stt: stt,
      curchapter: 1,
      curcvar: cvar
    })
  }

  async load_solution(pth: string) {
    const rawdata = await (await fetch(pth)).arrayBuffer();
    const compressed_scores = new Uint8Array(rawdata);
    if (this.state.curcvar) {
      this.state.curcvar.free();
    }
    if (this.state.stt) {
      this.state.stt.free();
    }
    const stt = new WState(
      fzstd.decompress(compressed_scores),
      JSON.stringify(this.state.books[this.state.curbook!].chapters));
    const cvar = stt.ini_cvar();
    this.setState({
      stt: stt,
      curchapter: 1,
      curcvar: cvar
    })
  }


  jump_to(dest: number, newcvar: WCharacterVariable) {
    this.setState({
      curchapter: dest,
      curcvar: newcvar
    })
  }

  outcome_button(pb: number, wns: WNS, score: number | undefined): JSX.Element {
    switch (wns.tp()) {
      case "lost": return <button type="button" className="btn btn-danger">{percent(pb)} - LOST!</button>
      case "win": return <button type="button" className="btn btn-success">{percent(pb)} - WON!</button>
    }
    var scorestr = "??";
    if (score) {
      scorestr = percent(score);
    }
    // we are in the chapter case
    const newchapter = wns.chapter()!;
    return <button
      type="button"
      key={wns.repr()}
      onClick={() => this.jump_to(newchapter.chapter, newchapter.cvar)}
      className="btn btn-default">
      {percent(pb)} / dst:{newchapter.chapter} / {cvar_diff(this.state.curcvar!, newchapter.cvar)} / [win:{scorestr}]
    </button>
  }

  solexplorer() {
    if (!this.state.stt || !this.state.curcvar) {
      return "Please load a solution!"
    }

    const ns = new WNS(this.state.curchapter, this.state.curcvar);
    const wchoices = this.state.stt.step(ns);

    var best_score = 0.0;
    var choices = [];
    for (var i = 0; i < wchoices.count(); i++) {
      const wchoice = wchoices.nth(i);
      if (!wchoice) {
        console.log("failed to load choice");
        continue;
      }
      const outcome = wchoice.res;
      if (!outcome) {
        console.log("failed to get outcome");
        continue;
      }

      var oos = [];
      var score = 0.0;
      for (var j = 0; j < outcome.count(); j++) {
        const pns = outcome.nth(j);
        if (!pns) {
          console.log("could not load PNS");
        } else {
          const curscore = this.state.stt.score(pns.v);
          if (curscore) {
            score += curscore * pns.p;
          }
          oos.push({ p: pns.p, wns: pns.v, score: curscore })
        }
      }
      outcome.free();
      if (score > best_score)
        best_score = score;

      choices.push({
        desc: wchoice.desc,
        oos: oos,
        score: score
      });
      wchoice.free();
    }
    ns.free();

    const curchapter = this.state.chapters[this.state.curchapter];

    return <div className="row">
      <div className="col col-md">
        <h3>Description</h3>
        <a href={"https://www.projectaon.org/en/xhtml/lw/" + urlpart(this.state.curbook!) + "/sect" + curchapter.title + ".htm"} target="_blank" rel="noopener noreferrer">Compare with original chapter</a><br/>
        {curchapter.desc}
      </div>
      <div className="col col-md">
        <h3>Encoded logic</h3>
        <div><pre>{JSON.stringify(curchapter.pchoice, null, 2)}</pre></div>
      </div>

      <div className="col col-md">
        <h3>Decisions</h3>
        <ul>{choices.map((c) => <li key={c.desc}>
          <span className={c.score >= best_score ? "badge badge-primary" : "badge badge-secondary"}>win:{percent(c.score)}</span> {c.desc}
          <ul>
            {c.oos.map((o) => <li key={o.p + o.wns.repr()}>{this.outcome_button(o.p, o.wns, o.score)}</li>)}
          </ul>
        </li>)}</ul>
      </div>
    </div >
  }

  solpath_load_button() {
    if (this.state.cursolpath) {
      const rsz = this.state.cursoldesc!.states / 5000000;
      const sz = rsz.toFixed(2);
      const withoutsol = <button type="button" className="btn btn-success" onClick={() => this.init_solution(this.state.cursolpath!)}>Run without solution</button>
      if (rsz < 2) {
        return <div>
          <button type="button" className="btn btn-danger" onClick={() => this.load_solution(this.state.cursolpath!)}>LOAD! Will use {sz}Gb</button>
          {withoutsol}
        </div>
      } else {
        return withoutsol
      }
    } else {
      return "";
    }
  }

  pscoredesc_button(d: PScoreDesc) {
    return <button
      type="button"
      key={d.path}
      className={this.state.cursolpath == d.path ? "btn btn-primary" : "btn btn-default"}
      onClick={() => this.set_soldesc(d.path)} >{percent(d.score)}</button>
  }

  set_book(bid: string) {
    var chapters: { [key: number]: Chapter } = {};
    for (var i = 0; i < this.state.books[bid].chapters.length; i++) {
      const [cid, c] = this.state.books[bid].chapters[i];
      chapters[cid] = c;
    }
    const content = this.state.books[bid].content;
    var descs: Array<PScoreDesc> = [];
    for (const path in content) {
      const curcontent = content[path];
      descs.push({
        path: path,
        desc: curcontent.desc,
        score: curcontent.score,
        states: curcontent.states,
      })
    }
    descs.sort((a, b) => b.score - a.score);
    this.setState({ curbook: bid, chapters: chapters, sorted_descs: descs })
  }

  cursoldesc_show() {
    if (!this.state.cursoldesc) {
      return "Not defined"
    }

    var s = this.state.cursoldesc!;

    if (this.state.curcvar) {
      return statedesc_show({
        score: s.score,
        states: s.states,
        chapter: this.state.curchapter,
        curhp: this.state.curcvar.endurance,
        maxhp: s.desc._ccst._maxendurance,
        items: this.state.curcvar.items.map(([i, q]) => {
          if (typeof i === 'string') {
            return [i, q]
          } else {
            return [JSON.stringify(i), q]
          }
        }),
        flags: this.state.curcvar.flags,
        disciplines: s.desc._ccst._discipline
      })
    } else {
      var items: Array<[string, number]> =
        s.desc._cvar._cvitems.map(([i, q]) => {
          if (typeof i === 'string') {
            return [i, q]
          } else {
            return [JSON.stringify(i), q]
          }
        });
      if (s.desc._cvar._cvgold > 0)
        items.push(["Gold", s.desc._cvar._cvgold])

      return statedesc_show({
        score: s.score,
        states: s.states,
        chapter: 1,
        curhp: s.desc._ccst._maxendurance,
        maxhp: s.desc._ccst._maxendurance,
        items: items,
        flags: s.desc._cvar._cvflags,
        disciplines: s.desc._ccst._discipline
      })
    }

  }

  render() {
    return (
      <div className="container-fluid">
        <div className="row">
          <h2>Solution selector</h2>
          <div className="col col-lg-2">{this.state.bookids.map((bid) =>
            <button
              type="button"
              key={bid}
              onClick={() => this.set_book(bid)}
              className={bid == this.state.curbook ? "btn btn-primary" : "btn btn-default"}>
              {bid} - {Object.keys(this.state.books[bid].content).length} sols
            </button>
          )}</div>
          <div className="col">
            <ul>
              {this.state.sorted_descs.map((d) => this.pscoredesc_button(d))}
            </ul>
          </div>
        </div>
        <div className="row">
          <h2>Player state</h2>
        </div>
        <div className="row">
          {this.cursoldesc_show()}
          {this.solpath_load_button()}
        </div>
        <div className="row">
          <h2>Solution explorer</h2>
        </div>
        {this.solexplorer()}
      </div >
    );
  }
}

export default App;
