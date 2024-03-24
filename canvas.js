window.addEventListener("load", function() {
  Scheme.load_main("canvas.wasm", {}, {
    document: {
      body() { return document.body; },
      getElementById: Document.prototype.getElementById.bind(document),
      createElement: Document.prototype.createElement.bind(document),
      createTextNode: Document.prototype.createTextNode.bind(document),
    },
    element: {
      value(elem) { return elem.value; },
      setValue(elem, value) { elem.value = value; },
      checked(elem) { return elem.checked; },
      setChecked(elem, checked) { elem.checked = (checked == 1); },
      setAttribute(elem, name, value) { elem.setAttribute(name, value); },
      removeAttribute(elem, name) { elem.removeAttribute(name); },
      appendChild(parent, child) { return parent.appendChild(child); },
      remove(elem) { elem.remove(); },
      replaceWith(oldElem, newElem) { oldElem.replaceWith(newElem); },
      addEventListener(elem, name, f) { elem.addEventListener(name, f); },
      removeEventListener(elem, name, f) { elem.removeEventListener(name, f); }
    },
    event: {
      target(event) { return event.target; }
    },
    canvas: {
      getContext(canvas) {return canvas.getContext("2d"); },
      fillRect(context, x, y, width, height) {
        context.fillRect(x, y, width, height);
      },
      fillStyle(context, style) {context.fillStyle = style; },
      getCursorPosition(canvas, event) { return [event.offsetX, event.offsetY]; }
    }
  });
});




