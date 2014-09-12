HTMLWidgets.widget({

  name: "dygraphs",

  type: "output",

  initialize: function(el, width, height) { 
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.dygraph)
      instance.dygraph.resize();
  },

  renderValue: function(el, x, instance) {
    
    // get dygraph attrs and populate file field
    var attrs = x.attrs;
    attrs.file = x.data;
    
    // resolve javascript functions
    this.resolveFunctions(x.attrs);
    
    // resolve "auto" legend behavior
    if (x.attrs.legend == "auto") {
      if (x.data.length <= 2)
        x.attrs.legend = "onmouseover";
      else
        x.attrs.legend = "always";
    }
    
    // provide an automatic x value formatter if none is already specified
    if (attrs.axes.x.valueFormatter === undefined)
      attrs.axes.x.valueFormatter = this.xValueFormatter(x.scale);
    
    // convert time to js time
    attrs.file[0] = attrs.file[0].map(function(value) { 
      var date = new Date(value);   
      return date;
    });
    
    // transpose array
    attrs.file = HTMLWidgets.transposeArray2D(attrs.file);
    
    // add drawCallback for group
    if (x.group != null)
      this.addGroupDrawCallback(x);  
      
    // add default font for viewer mode
    if (this.queryVar("viewer_pane") === "1")
      document.body.style.fontFamily = "Arial, sans-serif";
    
    if (instance.dygraph) { // update exisigng instance
       
      instance.dygraph.updateOptions(attrs);
    
    } else {  // create new instance
      
      // inject css if necessary
      if (x.css != null) {
        var style = document.createElement('style');
        style.type = 'text/css';
        if (style.styleSheet) 
          style.styleSheet.cssText = x.css;
        else 
          style.appendChild(document.createTextNode(x.css));
        document.getElementsByTagName("head")[0].appendChild(style);
      }
      
      // create the instance and add it to it's group (if any)
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
    this.evaluateStringMember(attrs, 'annotationClickHandler');
    this.evaluateStringMember(attrs, 'annotationDblClickHandler');
    this.evaluateStringMember(attrs, 'annotationMouseOutHandler');
    this.evaluateStringMember(attrs, 'annotationMouseOverHandler');
    this.evaluateStringMember(attrs, 'axisLabelFormatter');
    this.evaluateStringMember(attrs, 'axes.attrs.axisLabelFormatter');
    this.evaluateStringMember(attrs, 'axes.y.axisLabelFormatter');
    this.evaluateStringMember(attrs, 'axes.y2.axisLabelFormatter');
    this.evaluateStringMember(attrs, 'axes.attrs.ticker');
    this.evaluateStringMember(attrs, 'axes.y.ticker');
    this.evaluateStringMember(attrs, 'axes.y2.ticker');
    this.evaluateStringMember(attrs, 'xValueParser');
    this.evaluateStringMember(attrs, 'clickCallback');
    this.evaluateStringMember(attrs, 'drawCallback');
    this.evaluateStringMember(attrs, 'highlightCallback');
    this.evaluateStringMember(attrs, 'pointClickCallback');
    this.evaluateStringMember(attrs, 'underlayCallback');
    this.evaluateStringMember(attrs, 'unhighlightCallback');
    this.evaluateStringMember(attrs, 'zoomCallback');
    this.evaluateStringMember(attrs, 'drawHighlightPointCallback');
    this.evaluateStringMember(attrs, 'drawPointCallback');
    this.evaluateStringMember(attrs, 'valueFormatter');
    this.evaluateStringMember(attrs, 'axes.attrs.valueFormatter');
    this.evaluateStringMember(attrs, 'axes.y.valueFormatter');
    this.evaluateStringMember(attrs, 'axes.y2.valueFormatter');
    this.evaluateStringMember(attrs, 'plotter');
    var thiz = this;
    if (attrs.series != null) {
      for (name in attrs.series) {
        var series = attrs.series[name];
        thiz.evaluateStringMember(series, 'plotter');
      }
    }
  },
  
  evaluateStringMember: function(o, member) {
    var parts = member.split('.');
    for(var i = 0, l = parts.length; i < l; i++) {
      var part = parts[i];
      if(o !== null && typeof o === "object" && part in o) {
        if (i == (l-1)) { // if we are at the end of the line then evalulate 
          if (typeof o[part] === "string")
            o[part] = eval("(" + o[part] + ")")
          else if (o[part] instanceof Array)
            o[part].map(function(value) { return eval("(" + value + ")"); })
        } else { // otherwise continue to next embedded object
          o = o[part];
        }
      }
      else  // part not found, no evaluation 
        return;
    } 
  },
  
  // Returns the value of a GET variable
  queryVar: function(name) {
    return decodeURI(window.location.search.replace(
      new RegExp("^(?:.*[&\\?]" +
                 encodeURI(name).replace(/[\.\+\*]/g, "\\$&") +
                 "(?:\\=([^&]*))?)?.*$", "i"),
      "$1"));
  }
});
