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
    
    // get dygraph attrs
    var attrs = x.attrs;
    
    // resolve javascript functions
    this.resolveFunctions(x.attrs);
    
    // provide an automatic x value formatter if none is already specified
    if (attrs.axes.x.valueFormatter === undefined)
      attrs.axes.x.valueFormatter = this.xValueFormatter(x.scale);
    
    // convert time to js time
    attrs.file[0] = attrs.file[0].map(function(value) { return new Date(value); })
    
    // transpose array
    attrs.file = HTMLWidgets.transposeArray2D(attrs.file);
    
    // add drawCallback for group
    if (x.group != null)
      this.addGroupDrawCallback(x);  
    
    // update or create as required
    if (instance.dygraph) {
      instance.dygraph.updateOptions(attrs);
    } else {
      instance.dygraph = new Dygraph(el, attrs.file, attrs);
      if (x.group != null)
        this.groups[x.group].push(instance.dygraph);
    }
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
  },
  
  groups: {},
  
  addGroupDrawCallback: function(x) {
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing drawCallback
    var prevDrawCallback = attrs["drawCallback"];
    
    this.groups[x.group] = this.groups[x.group] || [];
    var group = this.groups[x.group];
    var blockRedraw = false;
    attrs.drawCallback = function(me, initial) {
      
      // call existing
      if (prevDrawCallback)
        prevDrawCallback(me, initial);
      
      // sync peers in group
      if (blockRedraw || initial) return;
      blockRedraw = true;
      var range = me.xAxisRange();
      for (var j = 0; j < group.length; j++) {
        if (group[j] == me) continue;
        group[j].updateOptions({
          dateWindow: range
        });
      }
      blockRedraw = false;
    };
  },
  
  resolveFunctions: function(attrs) {
    this.stringToFunction(attrs, 'annotationClickHandler');
    this.stringToFunction(attrs, 'annotationDblClickHandler');
    this.stringToFunction(attrs, 'annotationMouseOutHandler');
    this.stringToFunction(attrs, 'annotationMouseOverHandler');
    this.stringToFunction(attrs, 'axisLabelFormatter');
    this.stringToFunction(attrs, 'axes.attrs.axisLabelFormatter');
    this.stringToFunction(attrs, 'axes.y.axisLabelFormatter');
    this.stringToFunction(attrs, 'axes.y2.axisLabelFormatter');
    this.stringToFunction(attrs, 'axes.attrs.ticker');
    this.stringToFunction(attrs, 'axes.y.ticker');
    this.stringToFunction(attrs, 'axes.y2.ticker');
    this.stringToFunction(attrs, 'xValueParser');
    this.stringToFunction(attrs, 'clickCallback');
    this.stringToFunction(attrs, 'drawCallback');
    this.stringToFunction(attrs, 'highlightCallback');
    this.stringToFunction(attrs, 'pointClickCallback');
    this.stringToFunction(attrs, 'underlayCallback');
    this.stringToFunction(attrs, 'unhighlightCallback');
    this.stringToFunction(attrs, 'zoomCallback');
    this.stringToFunction(attrs, 'drawHighlightPointCallback');
    this.stringToFunction(attrs, 'drawPointCallback');
    this.stringToFunction(attrs, 'valueFormatter');
    this.stringToFunction(attrs, 'axes.attrs.valueFormatter');
    this.stringToFunction(attrs, 'axes.y.valueFormatter');
    this.stringToFunction(attrs, 'axes.y2.valueFormatter');
  },
  
  stringToFunction: function(o, member) {
    var parts = member.split('.');
    for(var i = 0, l = parts.length; i < l; i++) {
      var part = parts[i];
      if(o !== null && typeof o === "object" && part in o) {
        if (i == (l-1)) { // if we are at the end of the line then evalulate 
          if (typeof o[part] === "string")
            o[part] = eval("(" + o[part] + ")"); 
        } else { // otherwise continue to next embedded object
          o = o[part];
        }
      }
      else  // part not found, no evaluation 
        return;
    } 
  }
});
