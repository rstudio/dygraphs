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
    
    // transpose array
    x.file = this.transposeArray(x.file);
    
    // update or create as required
    if (instance.dygraph)
      instance.dygraph.updateOptions(x);
    else
      instance.dygraph = new Dygraph(el, x.file, x);
  },
  
  transposeArray: function(array) {
    var newArray = array[0].map(function(col, i) { 
      return array.map(function(row) { 
        return row[i] 
      })
    });
    return newArray;
  }
  
});
