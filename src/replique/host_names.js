// The names a JavaScript object has, for completing js/console.lo, gstr/tri,
// Long.fromN or (.toUpp "x") in a .cljs buffer. doc/protocol.md, the
// :completions op.
//
// Evaluated as raw JavaScript through replique.cljs/eval-js, in the runtime,
// because the runtime is the only thing that knows: a JavaScript object carries
// no declaration of what is in it, and what js/window holds depends on the page.
// It answers EDN as a string, for reload_css.js's reason - what comes back is
// what the runtime PRINTED, so the JVM reads it twice.
//
// The answer is two vectors, the names whose value is a function and the rest:
//
//   [["log" "warn" ...] ["memory" ...]]
//
// NOTHING IS CALLED, and no getter is run to find out what a name is. A
// property is classified by its descriptor, so reading what document has does
// not run document's getters - and one that is a getter is a field, which is
// what reading it is. The objects on the way there ARE read, because naming
// them is what the text did: js/document.body.cl reads document.body, as the
// code being written will.
//
// THE SPEC SAYS WHERE TO START, rather than this being handed an expression to
// evaluate. What names an object is decided on the JVM, where the compiler says
// what an alias stands for, and what travels is data - so no text out of
// somebody's buffer is ever evaluated as code:
//
//   {root: "global"}                         globalThis, which is js/
//   {root: "ns", name: "goog.string"}        a Closure provide, as base.js
//                                            registered it
//   {root: "module", name: "react"}          a JavaScript module the namespace
//                                            required - from the module system's
//                                            own cache, since the namespace that
//                                            asks has imported it already
//   {root: "value", value: "x"}              a literal, which is its own object
//
// and a path of property names to walk from there.

(async function (spec) {
  let o;
  switch (spec.root) {
  case "global": o = globalThis; break;
  case "ns": o = globalThis.$CLJS && $CLJS.namespaces.get(spec.name); break;
  case "module": o = globalThis.$CLJS && (await $CLJS.requireJs(spec.name)).$module; break;
  case "value": o = spec.value; break;
  }
  for (const p of spec.path) {
    if (o === null || o === undefined) break;
    o = o[p];
  }

  const fns = [], others = [], seen = new Set();
  // Every name a read would find, the prototype chain included: "x".toUpperCase
  // is String.prototype's and is what somebody writing (.toUpp "x") wants.
  // Object() so that a string or a number has a chain to walk.
  //
  // Short of Object.prototype, which every chain ends in: its hasOwnProperty
  // and __lookupGetter__ are on everything, and offering them under every
  // object buries what that object has - js/con answering js/constructor
  // before js/console. Unless it is what was asked about.
  if (o !== null && o !== undefined) {
    const top = Object(o) === Object.prototype ? null : Object.prototype;
    for (let x = Object(o); x !== null && x !== top; x = Object.getPrototypeOf(x)) {
      for (const k of Object.getOwnPropertyNames(x)) {
        // A name that could not be written back is not offered: what goes in
        // the buffer is a symbol, and o["my key"] is not one
        if (seen.has(k) || k === "constructor" || !/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(k)) continue;
        seen.add(k);
        const d = Object.getOwnPropertyDescriptor(x, k);
        (d && "value" in d && typeof d.value === "function" ? fns : others).push(k);
      }
    }
  }

  function vector(xs) { return "[" + xs.map(function (x) { return JSON.stringify(x); }).join(" ") + "]"; }
  return "[" + vector(fns) + " " + vector(others) + "]";
})
