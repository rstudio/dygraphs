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
    
    // provide an automatic x value formatter if none is already specified
    if (x.axes.x.valueFormatter === undefined)
      x.axes.x.valueFormatter = this.xValueFormatter(x.meta.scale);
    
    // convert time to js time
    x.file[0] = x.file[0].map(function(value) { return new Date(value); })
    
    // transpose array
    x.file = HTMLWidgets.transposeArray2D(x.file);
    
    // update or create as required
    if (instance.dygraph)
      instance.dygraph.updateOptions(x);
    else
      instance.dygraph = new Dygraph(el, x.file, x);
  },
  
  xValueFormatter: function(scale) {
    
    var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                      
    return function(millis) {
      var date = new Date(millis);
        if (scale == "yearly")
          return date.getUTCFullYear();
        else if (scale == "monthly" || scale == "quarterly")
          return monthNames[date.getUTCMonth()] + ' ' + date.getUTCFullYear(); 
        else if (scale == "daily" || scale == "weekly")
          return monthNames[date.getUTCMonth()] + ' ' + 
                           date.getUTCDate() + ' ' + 
                           date.getUTCFullYear();
        else
          return date.toUTCString();
    }
  }
});
