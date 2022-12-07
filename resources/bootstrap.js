// Reusable browser REPL bootstrapping. Patches the essential functions
// in goog.base to support re-loading of namespaces after page load.

// Monkey-patch goog.provide if running under optimizations :none

// Notice how this file only has a dependency on goog.base. This make
// it possible to connect to the cljs REPL from any environment, even environments without a
// clojurescript or google-closure dependency

(function() {
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

  // Patch goog.Dependency.prototype.load to add a crossorigin parameter to scripts
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

    if (!goog.inHtmlDocument_()) {
      goog.logToConsole_(
        'Cannot use default debug loader outside of HTML documents.');
      if (this.relativePath == 'deps.js') {
        // Some old code is relying on base.js auto loading deps.js failing with
        // no error before later setting CLOSURE_IMPORT_SCRIPT.
        // CLOSURE_IMPORT_SCRIPT should be set *before* base.js is loaded, or
        // CLOSURE_NO_DEPS set to true.
        goog.logToConsole_(
          'Consider setting CLOSURE_IMPORT_SCRIPT before loading base.js, ' +
            'or setting CLOSURE_NO_DEPS to true.');
        controller.loaded();
      } else {
        controller.pause();
      }
      return;
    }

    /** @type {!HTMLDocument} */
    var doc = goog.global.document;

    // If the user tries to require a new symbol after document load,
    // something has gone terribly wrong. Doing a document.write would
    // wipe out the page. This does not apply to the CSP-compliant method
    // of writing script tags.
    if (doc.readyState == 'complete' &&
        !goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING) {
      // Certain test frameworks load base.js multiple times, which tries
      // to write deps.js each time. If that happens, just fail silently.
      // These frameworks wipe the page between each load of base.js, so this
      // is OK.
      var isDeps = /\bdeps.js$/.test(this.path);
      if (isDeps) {
        controller.loaded();
        return;
      } else {
        throw Error('Cannot write "' + this.path + '" after document load');
      }
    }

    var nonce = goog.getScriptNonce_();
    if (!goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING &&
        goog.isDocumentLoading_()) {
      var key;
      var callback = function(script) {
        if (script.readyState && script.readyState != 'complete') {
          script.onload = callback;
          return;
        }
        goog.Dependency.unregisterCallback_(key);
        controller.loaded();
      };
      key = goog.Dependency.registerCallback_(callback);

      var defer = goog.Dependency.defer_ ? ' defer' : '';
      var nonceAttr = nonce ? ' nonce="' + nonce + '"' : '';
      var script = '<script crossorigin="anonymous" src="' + this.path + '"' + nonceAttr + defer +
          ' id="script-' + key + '"><\/script>';

      script += '<script' + nonceAttr + '>';

      if (goog.Dependency.defer_) {
        script += 'document.getElementById(\'script-' + key +
          '\').onload = function() {\n' +
          '  goog.Dependency.callback_(\'' + key + '\', this);\n' +
          '};\n';
      } else {
        script += 'goog.Dependency.callback_(\'' + key +
          '\', document.getElementById(\'script-' + key + '\'));';
      }

      script += '<\/script>';

      doc.write(
        goog.TRUSTED_TYPES_POLICY_ ?
          goog.TRUSTED_TYPES_POLICY_.createHTML(script) :
          script);
    } else {
      var scriptEl =
          /** @type {!HTMLScriptElement} */ (doc.createElement('script'));
      scriptEl.defer = goog.Dependency.defer_;
      scriptEl.async = false;

      // If CSP nonces are used, propagate them to dynamically created scripts.
      // This is necessary to allow nonce-based CSPs without 'strict-dynamic'.
      if (nonce) {
        scriptEl.nonce = nonce;
      }

      scriptEl.onload = function() {
        scriptEl.onload = null;
        controller.loaded();
      };

      scriptEl.src = goog.TRUSTED_TYPES_POLICY_ ?
        goog.TRUSTED_TYPES_POLICY_.createScriptURL(this.path) :
        this.path;
      doc.head.appendChild(scriptEl);
    }
  };

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

  // we must reuse Closure library dev time dependency management,
  // under namespace reload scenarios we simply delete entries from
  // the correct private locations
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
