window.addEventListener("load", function() {
  Scheme.load_main("sxml.wasm", {}, {
    document: {
      body() { return document.body; },
      createElement: Document.prototype.createElement.bind(document),
      createTextNode: Document.prototype.createTextNode.bind(document),
    },
    element: {
      setAttribute(elem, name, value) { elem.setAttribute(name, value); },
      appendChild(parent, child) { return parent.appendChild(child); }
    }
  });
});
