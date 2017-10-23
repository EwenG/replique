// Reusable browser REPL bootstrapping. Patches the essential functions
// in goog.base to support re-loading of namespaces after page load.

// Monkey-patch goog.provide if running under optimizations :none

// Notice how this file only has a dependency on goog.base. This make
// it possible to connect to the cljs REPL from any environment, even environments without a
// clojurescript or google-closure dependency

(function() {
  var loadQueue = null;

  if(!COMPILED) {
      
    var objremove = function(obj, key) {
      var rv;
      if (rv = key in  (obj)) {
        delete obj[key];
      }
      return rv;
    };

    var forceReload = function(src) {
      // cljs.core is not compatible with reloading. For example, Atom does not extend IReset
      // reset! check the type of the first argument instead. Reloading cljs.core would require
      // to recreate all already instantiated atoms
      if(!(src === "cljs.core")) {
        var path = goog.dependencies_.nameToPath[src];
        objremove(goog.dependencies_.visited, path);
        objremove(goog.dependencies_.written, path);
        objremove(goog.dependencies_.written, goog.basePath + path);
        if(goog.cljsReloadAll_) {
          for(src in goog.dependencies_.requires[path]) {
            forceReload(src);
          }
        }
      }
    };

    goog.require__ = goog.require;
    // suppress useless Google Closure error about duplicate provides
    goog.isProvided_ = function(name) {
      return false;
    };
    // provide cljs.user
    goog.constructNamespace_("cljs.user");
    goog.writeScriptTag__ = function(src, opt_sourceText) {
      // the page is already loaded, we can no longer leverage
      // document.write instead construct script tag elements and append
      // them to the body of the page, to avoid parallel script loading
      // enforce sequential load with a simple load queue
      var loaded = false;
      var onload = function() {
        if(loadQueue && !loaded) {
          loaded = !loaded;
          if(loadQueue.length === 0) {
            loadQueue = null;
            return null;
          } else {
            return goog.writeScriptTag__.apply(null, loadQueue.shift());
          }
        }
      }
      var script = document.createElement("script");
      script.type = "text/javascript";
      script.onload = onload;
      script.async = false;
      script.onreadystatechange = onload; //IE
      if(!opt_sourceText) {
        script.src = src;
      } else {
        script.textContent = opt_sourceText; // IE9 compatible
      }
      return document.body.appendChild(script);
    }
    goog.writeScriptTag_ = function(src, opt_sourceText) {
      if(loadQueue) {
        return loadQueue.push([src, opt_sourceText]);
      } else {
        loadQueue = [];
        return goog.writeScriptTag__(src, opt_sourceText);
      }
    };
    // we must reuse Closure library dev time dependency management,
    // under namespace reload scenarios we simply delete entries from
    // the correct private locations
    goog.require = function(src, reload) {
      if(reload === "reload-all") {
        goog.cljsReloadAll_ = true;
      }
      var maybeReload = reload || goog.cljsReloadAll__;
      if (maybeReload) {
        forceReload(src);
      }
      var ret = goog.require__(src);
      if(reload === "reload-all") {
        goog.cljsReloadAll_ = false;
      }
      return ret;
    }
  }
})();

