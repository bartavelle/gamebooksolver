var mytest = null;
import Alpine from 'alpinejs';

function format_books(books) {
  var out = [];
  for (const bookid in books) {
    var curbook = books[bookid];
    var content = [];
    for (const path in curbook.content) {
      content.push({ path: path, desc: curbook.content[path].desc, score: curbook.content[path].score })
    }
    content.sort((a, b) => b.score - a.score);
    curbook.content = content;
    out.push({ id: bookid, book: curbook })
  }
  return out
}

async function init_index() {
  const books = format_books(await (await fetch('/books.json')).json());
  Alpine.store('books', books)
  Alpine.start();
}

function percent(score) {
  if (!score) {
    return "??"
  }
  return (score * 100).toFixed(2) + "%"
}

function show_item(item) {
  if (typeof item === 'string' || item instanceof String)
    return item;
  else
    return JSON.stringify(item);
}

function diff_desc(cvar, outcome) {
  console.log("diff_desc", outcome)
  const ncvar = outcome.cvar;
  var desc = [];
  if(cvar.endurance != ncvar.endurance){
    desc.push("hp:" + ncvar.endurance)
  }

  var adesc = desc.join(" ")
  if(!adesc) {
    adesc = "(no change)"
  }
  return "ch:" + outcome.dest + " - " + adesc
}

async function init_explore() {
  const gswasm = await import("../pkg/index.js");
  gswasm.init();

  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);

  const path = urlParams.get("path");
  const bookid = urlParams.get("book");

  const books = await (await fetch('/books.json')).json();
  const desc = books[bookid].content[path].desc;
  const lchapters = books[bookid].chapters;
  const achapters = JSON.stringify(lchapters);

  var chapters = {};
  for (const i in lchapters) {
    const v = lchapters[i];
    chapters[v[0]] = v[1];
  }

  const rawdata = await (await fetch(path)).arrayBuffer();
  const encoded_scores = new Uint8Array(rawdata);

  const stt = new gswasm.WState(encoded_scores, achapters);
  window.ASTT = stt

  function step(curchapter, curcvar) {
    const rchoices = stt.step(new gswasm.WNS(curchapter, curcvar));
    var out = [];
    for (var i = 0; i < rchoices.count(); i++) {
      var c = rchoices.nth(i);
      var desc = c.desc;
      var routcome = c.res;
      var outcome = [];
      for (var j = 0; j < routcome.count(); j++) {
        var o = routcome.nth(j);
        const tp = o.v.tp();
        var v = {tp: tp};
        switch(tp) {
          case "lost": break;
          case "won": v['score'] = sc.won();
          case "chapter":
            const newchapter = o.v.chapter();
            v['dest'] = newchapter.chapter;
            v['cvar'] = newchapter.cvar;
        }
        outcome.push({ proba: o.p, ns: v });
      }
      out.push({ desc: desc, outcome: outcome });
    }
    console.log("out", out)
    return out;
  }

  Alpine.store('stt', stt)
  Alpine.store('desc', desc)
  Alpine.store('percent', percent)
  Alpine.store('show_item', show_item)
  Alpine.store('chapters', chapters)
  Alpine.store('step', step)
  Alpine.store('diff_desc', diff_desc)
  Alpine.start();
}

if (window.location.pathname === "/explore.html") {
  init_explore();
} else {
  init_index();
}

// http://localhost:8080/explore.html?book=Book02&path=data/B02/2010SW.TR.MO.HU.6Sg22-Sword-Shield.compact