function close_alert() {
  alert("clicked")
}


HTMLWidgets.widget({

  name: 'windowstack',

  type: 'output',

  factory: function(el, width, height) {

    var grid;
    var defaults;

    return {

      renderValue: function(x) {
        defaults = x.defaults;

        el.classList.add("grid-stack");
        el.classList.add("grid-stack-edit");
        el.id = x.id;
        el.innerHTML = x.html;

        grid = GridStack.init(x.options, el);
        grid.on("resizestop", function(event, el) {
          window.dispatchEvent(new Event("resize"));
        });

        if (HTMLWidgets.shinyMode) {
          var $all = $(el);
          Shiny.bindAll($all);
        }
        grid.on("added", function(event, items) {
          if (HTMLWidgets.shinyMode) {
            items.forEach(function(item) {
              var $item = $(item);
              Shiny.bindAll($item);
            });
          }
        });

        if (HTMLWidgets.shinyMode) {
          var serializedFull = grid.save(true, true);
          Shiny.setInputValue(el.id, serializedFull);
          grid.on("added removed change", function(event, items) {
            serializedFull = grid.save(true, true);
            Shiny.setInputValue(el.id, serializedFull);
          });
        }
      },

      getWidget: function() {
        return grid;
      },

      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      },

      addWidget: function(w) {
        return grid.addWidget(w);
      }
    };
  }
});
