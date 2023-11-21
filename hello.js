window.addEventListener("load", function() {
  Scheme.load_main("hello.wasm", {}, {
    document: {
      body() { return document.body; },
      createTextNode: Document.prototype.createTextNode.bind(document)
    },
    element: {
      appendChild(parent, child) { return parent.appendChild(child); }
    }
  });
});
