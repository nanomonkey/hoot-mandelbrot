window.addEventListener("load", function() {
  Scheme.load_main("todo.wasm", {}, {
    document: {
      body() { return document.body; },
      getElementById: Document.prototype.getElementById.bind(document),
      createElement: Document.prototype.createElement.bind(document),
      createTextNode: Document.prototype.createTextNode.bind(document),
      createTreeWalker: Document.prototype.createTreeWalker.bind(document)
    },
    element: {
      value(elem) { return elem.value; },
      setValue(elem, value) { elem.value = value; },
      setAttribute(elem, name, value) { elem.setAttribute(name, value); },
      removeAttribute(elem, name) { elem.removeAttribute(name); },
      appendChild(parent, child) { return parent.appendChild(child); },
      remove(elem) { elem.remove(); },
      replaceWith(oldElem, newElem) { oldElem.replaceWith(newElem); },
      addEventListener(elem, name, f) { elem.addEventListener(name, f); },
      removeEventListener(elem, name, f) { elem.removeEventListener(name, f); }
    },
    treeWalker: {
      currentNode(walker) { return walker.currentNode; },
      setCurrentNode(walker, node) { walker.currentNode = node; },
      nextNode(walker) { return walker.nextNode(); },
      firstChild(walker) { return walker.firstChild(); },
      nextSibling(walker) { return walker.nextSibling(); }
    }
  });
});
