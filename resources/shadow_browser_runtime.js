(function() {
  // At this point, goog/base.js has been evaluated and all goog modules
  // have been inlined. goog.provide and goog.require are the real ones
  // from base.js. We now override goog.require to a no-op because all
  // actual loading is handled by SHADOW_ENV.load() which loads modules
  // in dependency order.
  //
  // SHADOW_ENV.load MUST remain synchronous because Shadow-CLJS compiled
  // code calls it and immediately references the loaded modules. Making
  // it async would require the Shadow-CLJS compiler to emit different
  // code (e.g. with callbacks or async/await).
  //
  // The goog.replique_loading__ flag and goog.replique_after_load_hook__
  // are coordination points for the REPL. Since loading is synchronous
  // here, goog.replique_loading__ is always false and the hook is never
  // needed — results are sent immediately from process-pending-eval.
  // They are initialized for compatibility with the unified REPL code.

  goog.require = function(name, reload) {
    // If we are inside a goog.module context, return the exports
    if (goog.isInModuleLoader_ && goog.isInModuleLoader_()) {
      try {
        return goog.module.get(name);
      } catch (e) {
        // Not a module or not loaded, return undefined
        return undefined;
      }
    }
    // For non-module contexts (like CLJS), we don't need to return anything
    return undefined;
  };

  // Initialize coordination flags for the REPL.
  // goog.replique_loading__ is never true in this runtime since
  // loading is synchronous, but the REPL checks it.
  goog.replique_loading__ = false;

  window.SHADOW_ENV = (function() {
    var loaded = {};

    function loadSync(url) {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', url, false);
      xhr.send();
      if (xhr.status === 200) {
        (0, eval)(xhr.responseText + '\n//# sourceURL=' + url);
      } else {
        throw new Error('SHADOW_ENV: Failed to load ' + url + ' (status: ' + xhr.status + ')');
      }
    }

    return {
      loaded: loaded,

      load: function(opts, names) {
        for (var i = 0; i < names.length; i++) {
          var name = names[i];
          if (!loaded[name]) {
            loaded[name] = true;
            loadSync(SHADOW_REPL_BASE_PATH + name);
          }
        }
        // Loading is synchronous, so it's already complete.
        // No need to set goog.replique_loading__ or call
        // goog.replique_after_load_hook__ — the REPL sees
        // goog.replique_loading__ as false and sends results
        // immediately.
      },

      reload: function(names) {
        for (var i = 0; i < names.length; i++) {
          delete loaded[names[i]];
        }
        this.load({}, names);
      },

      setLoaded: function(name) {
        loaded[name] = true;
      }
    };
  })();
})();
