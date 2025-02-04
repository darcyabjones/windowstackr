// Resources
// https://unleash-shiny.rinterface.com/shiny-custom-handler


HTMLWidgets.widget({

  name: 'windowstack',

  type: 'output',

  factory: function(el, width, height) {

    let grid = null;
    let item_defaults = null;

    return {
      getGrid: function() {
        return grid;
      },
      getItemDefaults: function() {
        return item_defaults;
      },
      renderValue: function(x) {
        item_defaults = x.item_defaults;

        el.classList.add("grid-stack");
        el.classList.add("grid-stack-edit");
        el.id = x.id;
        el.innerHTML += x.html;

        if (x.head) {
          document.head.innerHTML += x.head;
        }

        grid = GridStack.init(x.options);
        grid.on("resizestop", function(event, el) {
          window.dispatchEvent(new Event("resize"));
        });

        if (HTMLWidgets.shinyMode) {
          let $all = $(el);
          Shiny.bindAll($all);
        }

        grid.on("added", function(event, items) {
          if (HTMLWidgets.shinyMode) {
            items.forEach(function(item) {
              let $item = $(item);
              Shiny.bindAll($item);
            });
          }
        });

        if (HTMLWidgets.shinyMode) {
          let serializedFull = grid.save(true, true);
          Shiny.setInputValue(el.id, serializedFull);
          grid.on("added removed change", function(event, items) {
            serializedFull = grid.save(true, true);
            Shiny.setInputValue(el.id, serializedFull);
          });
        }
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
        // I think this is already handled by gridstacks own callbacks though.
      }
    };
  }
});

function generateID() {
  return Date.now().toString(36) + Math.random().toString(36).substring(2, 12).padStart(12, 0);
}

function isNull(x) {
  return Object.is(x, undefined) || Object.is(x, null);
}

function getWidget(id = null) {
  if (!id) {
    element = document.querySelector(".grid-stack");
    id = "#" + element.id;
  }

  let htmlWidgetsObj = HTMLWidgets.find(id);
  let widgetObj;
  if (typeof htmlWidgetsObj !== "undefined") {
    widgetObj = htmlWidgetsObj.getGrid();
  }
  return widgetObj;
}

function windowClose(id, selectors = null) {
  if (!selectors) {
    selectors = [".grid-stack-item", ".windowstack-window"];
  }
  let wdw = null;
  let btn = document.querySelector(id);
  if (Array.isArray(selectors)) {
    for (sel of selectors) {
      wdw = btn.closest(sel);
      if (wdw) {break}
    }
  } else {
    wdw = btn.closest(selectors);
  }
  let grid_element = document.querySelector(id).closest(".grid-stack");
  if (!(isNull(wdw) || isNull(grid_element))) {
    gridstackRemoveElement({id: "#" + grid_element.id, element_id: "#" + wdw.id});
  } else {
    console.log("Tried to remove window from close button " + id + "but couldn't find something.")
    console.log(wdw);
    console.log(grid_element);
  }
}


function windowFullScreen(id, selectors = null) {
  if (!selectors) {
    selectors = [".grid-stack-item", ".windowstack-window"];
  }

  let wdw = null;
  let btn = document.querySelector(id);
  if (Array.isArray(selectors)) {
    for (sel of selectors) {
      wdw = btn.closest(sel);
      if (wdw) {break}
    }
  } else {
    wdw = btn.closest(selectors);
  }

  let fs_element = document.querySelector(id).closest(".windowstack-window");

  toggleWindowFullScreen(btn);
}

function toggleWindowFullScreen(el) {
  let icns = el.getElementsByTagName('i');
  var from;
  var to;

  let ae = el.getAttribute('aria-expanded');
  if (ae == "true") {
    el.setAttribute("aria-expanded",  false);
    from = "fa-down-left-and-up-right-to-center";
    to = "fa-up-right-and-down-left-from-center";
  } else {
    el.setAttribute("aria-expanded",  true);
    from = "fa-up-right-and-down-left-from-center";
    to = "fa-down-left-and-up-right-to-center";
  }

  for (icn of icns) {
    icn.classList.replace(from, to);
    icn.setAttribute("aria-label", to + " icon");
  }
}

function gridstackRemoveElement(x) {
  let grid = getWidget(x.id);
  let el = document.querySelector("#" + x.element_id);
  grid.removeWidget(el, removeDOM = true);
}

function gridstackRemoveAll(x) {
  let grid = getWidget(x.id);
  grid.removeAll(removeDOM = true);
}

function gridstackAddElement(x) {
  let grid = getWidget(x.id);
  let options = x.options;
  if (!options) {
    options = grid.getItemDefaults();
  }
  if (options) {
    grid.addWidget(x.element, options);
  } else {
    grid.addWidget(x.element);
  }
}

function gridstackAddElements(x) {
  let grid = getWidget(x.id);
  grid.batchUpdates(false);
  let element = null;
  let options = null;
  for (obj of x.objects) {
    element = object.element;
    options = object.options;
    if (isNull(options)) {
      options = grid.getItemDefaults();
    }
    if (isNull(options.id)) {
      options.id = object.id
    }
    if (isNull(object.id) && isNull(options.id)) {
      options.id = generateID();
    }

    grid.addWidget(element, options);
  }
  grid.batchUpdates(true);
}

function gridstackMakeElement(x) {
  let grid = getWidget(x.id);
  grid.makeWidget(x.element_id);
}

function gridstackMakeElements(x) {
  let grid = getWidget(x.id);
  grid.batchUpdate(false);
  for (id of x.element_ids) {
    grid.makeWidget(id);
  }
  grid.batchUpdate(true);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#columncolumn-number-layout-columnoptions--movescale
function gridstackSetColumn(x) {
  let grid = getWidget(x.id);
  grid.column(x.column, x.layout);
}

function gridstackGetColumn(x) {
  let grid = getWidget(x.id);
  grid.getColumn(); // TODO, make available to shiny
}

function gridstackSetFloat(x) {
  let grid = getWidget(x.id);
  grid.float(x.float);
}

function gridstackSetMargin(x) {
  let grid = getWidget(x.id);
  grid.margin(x.margin);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#compactlayout-compactoptions--compact-dosort--true
function gridstackCompact(x) {
  let grid = getWidget(x.id);
  grid.compact(x.layout, x.doSort);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#enablemovedoenable
function gridstackSetMove(x) {
  let grid = getWidget(x.id);
  grid.enableMove(x.doEnable);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#enableresizedoenable
function gridstackSetResize(x) {
  let grid = getWidget(x.id);
  grid.enableResize(x.doEnable);
}

function gridstackSetStatic(x) {
  let grid = getWidget(x.id);
  grid.setStatic(x.staticValue);
}

function gridstackUpdateElement(x) {
  let grid = getWidget(x.id);
  let el = document.querySelector(x.element_id);
  grid.update(el, x.options);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#movableel-val
function gridstackSetElementMovable(x) {
  let grid = getWidget(x.id);
  let el = document.querySelector(x.element_id);
  grid.movable(el, x.val);
}

//https://github.com/gridstack/gridstack.js/tree/master/doc#resizableel-val
function gridstackSetElementResizable(x) {
  let grid = getWidget(x.id);
  let el = document.querySelector(x.element_id);
  grid.resizable(el, x.val);
}


if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("gridstack_make_element", gridstackMakeElement);
  Shiny.addCustomMessageHandler("gridstack_make_elements", gridstackMakeElements);
  Shiny.addCustomMessageHandler("gridstack_add_element", gridstackAddElement);
  Shiny.addCustomMessageHandler("gridstack_add_elements", gridstackAddElements);
  Shiny.addCustomMessageHandler("gridstack_set_column", gridstackSetColumn);
  Shiny.addCustomMessageHandler("gridstack_set_float", gridstackSetFloat);
  Shiny.addCustomMessageHandler("gridstack_set_margin", gridstackSetMargin);
}
