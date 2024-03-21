window.addEventListener("load", function() {
  Scheme.load_main("canvas.wasm", {}, {
    document: {
      body() { return document.body; },
      getElementById: Document.prototype.getElementById.bind(document)
    },
    canvas: {
       getContext(canvas) {
        return canvas.getContext("2d");
      },
      fillRect(context, x, y, width, height) { 
        context.fillRect(x, y, width, height);
      },
      fillStyle(context, style) { 
        context.fillStyle = style;
      }
    }
  });
});
