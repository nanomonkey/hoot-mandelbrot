window.addEventListener("load", function() {
  Scheme.load_main("counter.wasm", {}, {
    document: {
      body() { return document.body; },
      getElementById: Document.prototype.getElementById.bind(document),
      createElement: Document.prototype.createElement.bind(document),
      createTextNode: Document.prototype.createTextNode.bind(document),
    },
    element: {
      setAttribute(elem, name, value) { elem.setAttribute(name, value); },
      appendChild(parent, child) { return parent.appendChild(child); },
      remove(elem) { elem.remove(); },
      addEventListener(elem, name, f) { elem.addEventListener(name, f); }
    }
  });
});
