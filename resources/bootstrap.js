// Reusable browser REPL bootstrapping. Patches the essential functions
// in goog.base to support re-loading of namespaces after page load.
//
// All script loading uses synchronous XHR instead of <script> injection.
// This makes goog.require synchronous so the REPL can evaluate JS
// immediately without deferred eval or after-load hooks.

(function() {
  // Set CLOSURE_IMPORT_SCRIPT before any goog.require calls.
  // Both the old path (goog.importScript_) and new path
  // (goog.Dependency.prototype.load) check this.
  goog.global.CLOSURE_IMPORT_SCRIPT = function(src, opt_sourceText) {
    if (opt_sourceText) {
      (0, eval)(opt_sourceText + '\n//# sourceURL=' + src);
    } else {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', src, false);
      xhr.send();
      if (xhr.status === 200) {
        (0, eval)(xhr.responseText + '\n//# sourceURL=' + src);
      } else {
        throw new Error('Failed to load ' + src + ' (status: ' + xhr.status + ')');
      }
    }
    return true;
  };

  goog.require__ = goog.require;
  goog.isProvided__ = goog.isProvided_;

  // Patch goog.isProvided_ to enable script reloading
  goog.isProvided_ = function(name) {
    if (!goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING &&
        goog.isDocumentLoading_()) {
      return goog.isProvided__(name);
    } else {
      return false;
    }
  };

  // Patch goog.Dependency.prototype.load to use CLOSURE_IMPORT_SCRIPT.
  // Covers the new Closure loading path used by goog.DebugLoader_.
  goog.Dependency.prototype.load__ = goog.Dependency.prototype.load;
  goog.Dependency.prototype.load = function(controller) {
    if (goog.global.CLOSURE_IMPORT_SCRIPT) {
      if (goog.global.CLOSURE_IMPORT_SCRIPT(this.path)) {
        controller.loaded();
      } else {
        controller.pause();
      }
      return;
    }
    throw new Error('CLOSURE_IMPORT_SCRIPT not set; cannot load ' + this.path);
  };

  // Patch goog.DebugLoader_.writeScriptTag_ to use CLOSURE_IMPORT_SCRIPT.
  // Covers the old Closure loading path — prevents <script> DOM node accumulation.
  if (typeof goog.DebugLoader_ !== 'undefined' &&
      goog.DebugLoader_.prototype &&
      goog.DebugLoader_.prototype.writeScriptTag_) {
    goog.DebugLoader_.prototype.writeScriptTag__ =
      goog.DebugLoader_.prototype.writeScriptTag_;
    goog.DebugLoader_.prototype.writeScriptTag_ = function(path, opt_sourceText) {
      if (goog.global.CLOSURE_IMPORT_SCRIPT) {
        return goog.global.CLOSURE_IMPORT_SCRIPT(path, opt_sourceText);
      }
      throw new Error('CLOSURE_IMPORT_SCRIPT not set; cannot load ' + path);
    };
  }

  var objremove = function(obj, key) {
    var rv;
    if (rv = key in (obj)) {
      delete obj[key];
    }
    return rv;
  };

  var forceReload = function(src) {
    var dependencies = [];
    var visitedDependencies = {};
    do {
      var path = goog.debugLoader_.getPathFromDeps_(src);
      if(!(src === "cljs.core") && !visitedDependencies[src]) {
        visitedDependencies[src] = true;
        objremove(goog.debugLoader_.written_, path);
        objremove(goog.debugLoader_.written_, goog.basePath + path);
        var requiresLength = goog.debugLoader_.dependencies_[path].requires ? goog.debugLoader_.dependencies_[path].requires.length : 0;
        for(var i = 0; i < requiresLength; i++) {
          dependencies.push(goog.debugLoader_.dependencies_[path].requires[i]);
        }
      }
      src = dependencies.pop();
    } while(goog.cljsReloadAll__ && dependencies.length > 0);
  };

  goog.require = function(src, reload) {
    if(reload == "reload-all") {
      goog.cljsReloadAll__ = true;
    }
    var maybeReload = reload || goog.cljsReloadAll__;
    if (maybeReload) {
      forceReload(src);
    }
    var ret = goog.require__(src);
    goog.cljsReloadAll__ = false;
    return ret;
  };
})();
