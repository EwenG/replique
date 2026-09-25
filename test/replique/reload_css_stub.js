// A document for reload_css.js to work on, for the tests of it.
//
// The script is plain JavaScript evaluated by the runtime, so it can be run on
// NODE - where there is no DOM at all - against a document written here. Real
// eval, real script, invented document: the same substitution the compiler's
// browser-test makes when it runs its browser client on a node process.
//
// THE SELECTOR IS READ RATHER THAN IGNORED. A stub that answered
// querySelectorAll with its whole list would make the selector in the script
// dead text, and the two tests that matter most about it - a <link rel=preload>
// is not a stylesheet, a <link> with no href is not one either - would pass
// against a script that asked for anything at all. So there is a matcher here,
// small enough to read and exact enough that changing the selector changes what
// comes back.
//
// Called with {origin, links: [{rel, href}], sheets: [...]} and leaves
// `location', `document' and `__stub' on globalThis. `__stub.nodes()' is what
// the document holds now, as EDN, and `__stub.fire(kind)' runs the load or error
// listeners the script registered - neither of which a browser would need, and
// both of which are how a test watches a swap finish.
(function (spec) {
  var nodes = [];

  function Node(attrs) {
    this.rel = attrs.rel;
    this.href = attrs.href;
    this.listeners = {};
  }
  Node.prototype.cloneNode = function () {
    return new Node({ rel: this.rel, href: this.href });
  };
  Node.prototype.addEventListener = function (kind, f) { this.listeners[kind] = f; };
  Node.prototype.after = function (n) {
    nodes.splice(nodes.indexOf(this) + 1, 0, n);
  };
  Node.prototype.remove = function () {
    var i = nodes.indexOf(this);
    if (i >= 0) nodes.splice(i, 1);
  };

  function matches(node, sel) {
    var tag = /^([a-z]+)/.exec(sel);
    if (!tag) throw new Error("the stub cannot read this selector: " + sel);
    if (tag[1] !== "link") return false;
    var parts = sel.slice(tag[1].length).match(/\[[^\]]*\]/g) || [];
    return parts.every(function (p) {
      var a = /^\[([\w-]+)(?:(~?=)"([^"]*)")?\]$/.exec(p);
      if (!a) throw new Error("the stub cannot read this selector: " + sel);
      var value = node[a[1]];
      if (value === undefined || value === null) return false;
      if (!a[2]) return true;
      if (a[2] === "=") return String(value) === a[3];
      return String(value).split(/\s+/).indexOf(a[3]) >= 0;
    });
  }

  nodes = spec.links.map(function (l) { return new Node(l); });

  globalThis.location = { origin: spec.origin, href: spec.origin + "/" };
  globalThis.document = {
    querySelectorAll: function (sel) {
      return nodes.filter(function (n) { return matches(n, sel); });
    },
    // What an @import'ed sheet is in, and what this script must not read: it has
    // no node, so there is nothing to swap, and replique 1 read this list and
    // had to filter it back down to the ones that did
    styleSheets: spec.sheets || []
  };
  function edn(x) { return x === undefined || x === null ? "nil" : JSON.stringify(x); }

  globalThis.__stub = {
    // EDN, read by the same two reads the script's own answer is read by -
    // there is a writer for JSON on the JVM side of replique and no reader
    nodes: function () {
      return "[" + nodes.map(function (n) {
        return "{:rel " + edn(n.rel) + " :href " + edn(n.href) + "}";
      }).join(" ") + "]";
    },
    fire: function (kind) {
      nodes.slice().forEach(function (n) {
        var f = n.listeners[kind];
        if (f) { delete n.listeners[kind]; f(); }
      });
    }
  };
  return "ready";
})
