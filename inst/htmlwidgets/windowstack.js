

HTMLWidgets.widget({

  name: 'windowstack',

  type: 'output',

  factory: function(el, width, height) {

    var grid = null;
    var defaults = null;

    return {

      renderValue: function(x) {
        defaults = x.defaults;

        el.classList.add("grid-stack");
        el.classList.add("grid-stack-edit");
        el.id = x.id;
        el.innerHTML = x.html;

        if (x.head) {
          document.head.innerHTML += x.head;
        }

        console.log(x.options);
        grid = GridStack.init(x.options);
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
        return grid
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

function getWidget(id) {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  var widgetObj;
  if (typeof htmlWidgetsObj !== "undefined") {
    widgetObj = htmlWidgetsObj.getWidget();
  }
  return widgetObj;
}

function gridstackCloseWindow(id) {
  wdw = document.querySelector(id).closest(".windowstack-window");
  grid_element = document.querySelector(id).closest(".grid-stack");
  var grid = getWidget(grid_element.id);
  grid.removeWidget(wdw, removeDOM = true);
}

function gridstackMakeWindow(x) {
  grid_element = document.querySelector("#" + x.id);
  var grid = getWidget(grid_element.id);
  console.log(grid);
  console.log(x.window_id);
  grid.makeWidget("#" + x.window_id);
}

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("gridstack_make_window", gridstackMakeWindow);
}
