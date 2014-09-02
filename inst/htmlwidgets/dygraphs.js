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
