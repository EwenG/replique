// Reusable browser REPL bootstrapping. Patches the essential functions
// in goog.base to support re-loading of namespaces after page load.

// Monkey-patch goog.provide if running under optimizations :none

// Notice how this file only has a dependency on goog.base. This make
// it possible to connect to the cljs REPL from any environment, even environments without a
// clojurescript or google-closure dependency

(function() {
  var loadQueue = null;
  // hook to wait for cljs files to be loaded
  goog.replique_loading__ = false;

  if(!COMPILED) {
      
    var objremove = function(obj, key) {
      var rv;
      if (rv = key in  (obj)) {
        delete obj[key];
      }
      return rv;
    };

    /*var forceReload = function(src) {
      var dependencies = [];
      var visitedDependencies = {};
      do {
        var path = goog.debugLoader_.idToPath_[src];
        if(!(src === "cljs.core") && !visitedDependencies[src]) {
          visitedDependencies[src] = true;
          //objremove(goog.debugLoader_.dependencies_.visited, path);
          objremove(goog.debugLoader_.written_, path);
          objremove(goog.debugLoader_.written_, goog.basePath + path);
          for(src in goog.debugLoader_.dependencies_[path].requires) {
            dependencies.push(src);
          }
        }
        src = dependencies.pop();
      } while(goog.cljsReloadAll_ && dependencies.length > 0);
      };*/

    var forceReload = function(src) {
      var dependencies = [];
      var visitedDependencies = {};
      do {
        var path = goog.debugLoader_.getPathFromDeps_(src);
        if(!(src === "cljs.core") && !visitedDependencies[src]) {
          visitedDependencies[src] = true;
          //objremove(goog.debugLoader_.dependencies_.visited, path);
          objremove(goog.debugLoader_.written_, path);
          objremove(goog.debugLoader_.written_, goog.basePath + path);
          for(src in goog.debugLoader_.dependencies_[path].requires) {
            dependencies.push(src);
          }
        }
        src = dependencies.pop();
      } while(goog.cljsReloadAll_ && dependencies.length > 0);
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
            if(goog.replique_after_load_hook__) {
              goog.replique_loading__ = false;
              goog.replique_after_load_hook__.call(null);
            }
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
        goog.replique_loading__ = true;
        loadQueue = [];
        return goog.writeScriptTag__(src, opt_sourceText);
      }
    };
    // In the latest Closure library implementation, there is no goog.writeScriptTag_,
    // to monkey-patch. The behavior of interest is instead in goog.Dependency.prototype.load,
    // which first checks and uses CLOSURE_IMPORT_SCRIPT if defined. So we hook our desired
    // behavior here.
    CLOSURE_IMPORT_SCRIPT = goog.writeScriptTag_;
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
