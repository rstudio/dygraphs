HTMLWidgets.widget({

  name: "dygraphs",

  type: "output",

  initialize: function(el, width, height) { 
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.dygraph)
      instance.dygraph.resize(width, height);
  },

  renderValue: function(el, x, instance) {
    
    // provide an automatic y value formatter if none is already specified
    var scale = x.meta.scale;
    var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    if (x.axes.x.valueFormatter === undefined) {
      x.axes.x.valueFormatter = function(millis) {
        if (scale == "yearly") {
          return new Date(millis).getUTCFullYear();
        } else if (scale == "monthly" || scale == "quarterly") {
          var date = new Date(millis);
          return monthNames[date.getUTCMonth()] + ' ' + date.getUTCFullYear(); 
        } else {
          return new Date(millis).toUTCString();
        }
      };
    }
    
    // convert time to js time
    x.file[0] = x.file[0].map(function(value) { return new Date(value); })
    
    // transpose array
    x.file = HTMLWidgets.transposeArray2D(x.file);
    
    // update or create as required
    if (instance.dygraph)
      instance.dygraph.updateOptions(x);
    else
      instance.dygraph = new Dygraph(el, x.file, x);
  } 
});
