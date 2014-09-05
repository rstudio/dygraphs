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
    
    // resolve javascript functions
    this.resolveFunctions(x);
    
    // provide an automatic x value formatter if none is already specified
    if (x.axes.x.valueFormatter === undefined)
      x.axes.x.valueFormatter = this.xValueFormatter(x.meta.scale);
    
    // convert time to js time
    x.file[0] = x.file[0].map(function(value) { return new Date(value); })
    
    // transpose array
    x.file = HTMLWidgets.transposeArray2D(x.file);
    
    // add drawCallback for group
    if (x.meta.group != null)
      this.addGroupDrawCallback(x);  
    
    // update or create as required
    if (instance.dygraph) {
      instance.dygraph.updateOptions(x);
    } else {
      instance.dygraph = new Dygraph(el, x.file, x);
      if (x.meta.group != null)
        this.groups[x.meta.group].push(instance.dygraph);
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
    this.groups[x.meta.group] = this.groups[x.meta.group] || [];
    var group = this.groups[x.meta.group];
    var blockRedraw = false;
    x.drawCallback = function(me, initial) {
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
  
  resolveFunctions: function(x) {
    this.evaluateMember(x, 'axisLabelFormatter');
    this.evaluateMember(x, 'axes.x.axisLabelFormatter');
    this.evaluateMember(x, 'axes.y.axisLabelFormatter');
  },
  
  evaluateMember: function(o, member) {
    var parts = member.split('.');
    for(var i = 0, l = parts.length; i < l; i++) {
      var part = parts[i];
      if(o !== null && typeof o === "object" && part in o) {
        if (i == (l-1)) // if we are at the end of the line then evalulate
          o[part] = eval("(" + o[part] + ")"); 
        else // otherwise continue to next embedded object
          o = o[part];
      }
      else  // part not found, no evaluation 
        return;
    } 
  }
});
