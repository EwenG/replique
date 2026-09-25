// Swapping a stylesheet for a fresh copy of itself, in every page connected to
// this process. doc/protocol.md, the :reload-css op.
//
// Evaluated as raw JavaScript through replique.cljs/eval-js - there is no
// ClojureScript anywhere in it - and it answers EDN, as a string, because that is
// what the runtime can carry back: the value of an evaluation is what the runtime
// PRINTED, so the JVM reads what comes back twice, once to take off the quoting
// print put on it and once for the map inside. The same rule runtime_browser.js
// follows in the other direction, and for the same reason: JSON.stringify of a
// string is a valid EDN string literal, the escapes EDN knows being a superset of
// the ones V8 emits.
//
// The answer, for a page that has the file:
//
//   {:reloaded ["http://localhost:8082/css/main.css"]
//    :stylesheets ["http://localhost:8082/css/main.css" ...]}
//
// WHAT THE PAGE HAS COMES BACK EVERY TIME, matched or not. Replique 1 needed two
// round trips - one op to list the stylesheets, one to reload the chosen one -
// and its worst message was "Could not find a css file to reload" with no hint of
// what the page actually had. The list is the hint, it is free, and it costs one
// op instead of two.
//
// FOUR THINGS ARE DONE DIFFERENTLY FROM REPLIQUE 1, and each of them is a bug it
// had:
//
// 1. THE MATCH IS THE LONGEST PATH SUFFIX, not the basename. `css/main.css' beats
//    a bare `main.css', so a project with a main.css per theme reloads the right
//    one, and every link that ties for longest is reloaded - which is the honest
//    answer when a page includes one file twice. Replique 1 matched basenames,
//    found several, and asked you which; the answer was remembered in a defvar
//    that died with the Emacs session.
//
// 2. THE MATCHING HAPPENS HERE, in the page, because the page is where the list
//    is. That is what makes one round trip enough.
//
// 3. A FRESH NODE IS PUT IN BESIDE THE OLD ONE AND THE OLD ONE GOES WHEN IT HAS
//    LOADED, rather than the href of the live node being rewritten. Rewriting it
//    unstyles the page for as long as the fetch takes, and leaves it unstyled for
//    good if the fetch 404s or the file no longer parses. The clone goes directly
//    after the original so the cascade order is unchanged, and it is the clone
//    that is dropped when the load fails - so a mistake costs nothing and the page
//    keeps the stylesheet that was working.
//
// 4. THE DOM IS READ, not document.styleSheets. There is no Closure Library here,
//    and the DOM is the better source anyway: a <link> whose stylesheet 404'd is
//    in the DOM and NOT in document.styleSheets, and that is precisely the one you
//    have just fixed and want reloaded.
//
// What is kept from replique 1: only <link>s, because an @import'ed sheet has no
// node to swap; and only this page's own origin, because a stylesheet served from
// somewhere else is not a file you are editing.
(function (want) {
  // Ours, and taken back off everything this reports, so that the same page
  // reloaded twice reports the same URL. Any other query the page put on its own
  // link is left alone - `v' is the one we overwrite, which is what a version
  // parameter is for.
  var BUST = "v";

  function url(href) { return new URL(href, location.href); }

  function shown(href) {
    var u = url(href);
    u.searchParams.delete(BUST);
    return u.href;
  }

  // The origin and not the hostname: a page on :8082 and an asset server on :3000
  // are two different origins, and the second one is not what you are editing.
  var links = Array.prototype.slice
    .call(document.querySelectorAll('link[rel~="stylesheet"][href]'))
    .filter(function (l) {
      try { return url(l.href).origin === location.origin; } catch (e) { return false; }
    });

  function segments(path) {
    return path.split("/").filter(Boolean).map(function (s) {
      try { return decodeURIComponent(s); } catch (e) { return s; }
    });
  }

  // How many trailing path segments this link shares with the file that changed.
  // Zero is no match at all: the name has to agree before anything else can.
  var wanted = segments(want);
  function score(link) {
    var have = segments(url(link.href).pathname);
    var n = 0;
    while (n < have.length && n < wanted.length &&
           have[have.length - 1 - n] === wanted[wanted.length - 1 - n]) {
      n = n + 1;
    }
    return n;
  }

  var scored = links.map(function (l) { return { link: l, n: score(l) }; });
  var best = scored.reduce(function (m, s) { return Math.max(m, s.n); }, 0);

  var reloaded = [];
  if (best > 0) {
    scored.filter(function (s) { return s.n === best; }).forEach(function (s) {
      var link = s.link;
      var fresh = link.cloneNode(false);
      var u = url(link.href);
      u.searchParams.set(BUST, String(Date.now()));
      fresh.href = u.href;
      fresh.addEventListener("load", function () { link.remove(); }, { once: true });
      fresh.addEventListener("error", function () { fresh.remove(); }, { once: true });
      link.after(fresh);
      reloaded.push(shown(link.href));
    });
  }

  function vector(xs) { return "[" + xs.map(JSON.stringify).join(" ") + "]"; }

  return "{:reloaded " + vector(reloaded) +
    " :stylesheets " + vector(links.map(function (l) { return shown(l.href); })) + "}";
})
