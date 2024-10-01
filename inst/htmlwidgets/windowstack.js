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
        el.innerHTML = x.html;

        grid = GridStack.init(x.options, el);
        grid.on("resizestop", function(event, el) {
          window.dispatchEvent(new Event("resize"));
        });

        //If (HTMLWidgets.shinyMode) {
        //  var $all = $(el);
        //  Shiny.bindAll($all);
        //}
        //Grid.on("added", function(event, items) {
        //  if (HTMLWidgets.shinyMode) {
        //    items.forEach(function(item) {
        //      var $item = $(item);
        //      Shiny.bindAll($item);
        //    });
        //  }
        //});

        //If (HTMLWidgets.shinyMode) {
        //  var serializedFull = grid.save(true, true);
        //  Shiny.setInputValue(el.id + "_layout", serializedFull);
        //  grid.on("added removed change", function(event, items) {
        //    serializedFull = grid.save(true, true);
        //    Shiny.setInputValue(el.id + "_layout", serializedFull);
        //  });
        //}

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
