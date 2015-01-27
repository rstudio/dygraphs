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
    
    // reference to this for closures
    var thiz = this;
    
    // get dygraph attrs and populate file field
    var attrs = x.attrs;
    attrs.file = x.data;
        
    // resolve "auto" legend behavior
    if (x.attrs.legend == "auto") {
      if (x.data.length <= 2)
        x.attrs.legend = "onmouseover";
      else
        x.attrs.legend = "always";
    }
    
    // set appropriated function in case of fixed tz
    if ((attrs.axes.x.axisLabelFormatter === undefined) && x.fixedtz)
      attrs.axes.x.axisLabelFormatter = this.xAxisLabelFormatterFixedTZ(x.tzone);
      
    if ((attrs.axes.x.valueFormatter === undefined) && x.fixedtz)
      attrs.axes.x.valueFormatter = this.xValueFormatterFixedTZ(x.scale, x.tzone);

    if ((attrs.axes.x.ticker === undefined) && x.fixedtz)
      attrs.axes.x.ticker = this.customDateTickerFixedTZ(x.tzone);
  
    // provide an automatic x value formatter if none is already specified
    if ((attrs.axes.x.valueFormatter === undefined) && (x.fixedtz != true))
      attrs.axes.x.valueFormatter = this.xValueFormatter(x.scale);
    
    // provide an automatic x axis label formatter if none is already specified
    // (to keep short year than full year (for dygraph 1.1.0)
    if ((attrs.axes.x.axisLabelFormatter === undefined) && (x.fixedtz != true))
      attrs.axes.x.axisLabelFormatter = this.xAxisLabelFormatter();
    
    // convert time to js time
    attrs.file[0] = attrs.file[0].map(function(value) {
      return thiz.normalizeDateValue(x.scale, value);
    });
    if (attrs.dateWindow != null) {
      attrs.dateWindow = attrs.dateWindow.map(function(value) {
        var date = thiz.normalizeDateValue(x.scale, value);
        return date.getTime();
      });
    }
    
    // transpose array
    attrs.file = HTMLWidgets.transposeArray2D(attrs.file);
    
    // add drawCallback for group
    if (x.group != null)
      this.addGroupDrawCallback(x);  
      
    // add shading and event callback if necessary
    this.addShadingCallback(x);
    this.addEventCallback(x);
      
    // add default font for viewer mode
    if (this.queryVar("viewer_pane") === "1")
      document.body.style.fontFamily = "Arial, sans-serif";
    
    if (instance.dygraph) { // update existing instance
       
      instance.dygraph.updateOptions(attrs);
    
    } else {  // create new instance
      
      // add shiny input for date window
      if (HTMLWidgets.shinyMode)
        this.addDateWindowShinyInput(el.id, x);
  
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
     
    // set annotations
    if (x.annotations != null) {
      instance.dygraph.ready(function() {
        x.annotations.map(function(annotation) {
          var date = thiz.normalizeDateValue(x.scale, annotation.x);
          annotation.x = date.getTime();
        });
        instance.dygraph.setAnnotations(x.annotations);
      }); 
    }
      
  },
  
  customDateTickerFixedTZ : function(tz){
    return function(t,e,a,i,r) {   
      var a=Dygraph.pickDateTickGranularity(t,e,a,i);
	    if(a >= 0){
        
        var n=i("axisLabelFormatter"),
        o=i("labelsUTC"),
        s=o?Dygraph.DateAccessorsUTC:Dygraph.DateAccessorsLocal;
        l=Dygraph.TICK_PLACEMENT[a].datefield;
        h=Dygraph.TICK_PLACEMENT[a].step;
        p=Dygraph.TICK_PLACEMENT[a].spacing;
        
        var y = [];
        var d = moment(t);
        d.tz(tz); 
        d.millisecond(0);
      
        if(l > 1){
          var x;
          if (l===5) {  // seconds 
            x = d.second();         
            d.second(x - x % h);     
          } else if(l===4){
            d.second(0)
            x = d.minute();
            d.minute(x - x % h);
          } else if(l===3){
            d.second(0);
            d.minute(0);
            x = d.hour();
            d.hour(x - x % h);
          } else if(l===2){
            d.second(0);
            d.minute(0);
            d.hour(0);
            if (h == 7) {  // one week
                d.startOf('week');
            }
          }
          
          v = d.valueOf();
          _=moment(v).tz(tz);
        
          // For spacings coarser than two-hourly, we want to ignore daylight
          // savings transitions to get consistent ticks. For finer-grained ticks,
          // it's essential to show the DST transition in all its messiness.
          var start_offset_min = moment(v).tz(tz).zone();
          var check_dst = (p >= Dygraph.TICK_PLACEMENT[Dygraph.TWO_HOURLY].spacing);
          
	        if(a<=Dygraph.HOURLY){
		        for(t>v&&(v+=p,_=moment(v).tz(tz));e>=v;){
			        y.push({v:v,label:n(_,a,i,r)});
			        v+=p;
			        _=moment(v).tz(tz);
		        }
	        }else{
            for(t>v&&(v+=p,_=moment(v).tz(tz));e>=v;){  
            
              // This ensures that we stay on the same hourly "rhythm" across
              // daylight savings transitions. Without this, the ticks could get off
              // by an hour. See tests/daylight-savings.html or issue 147.
              if (check_dst && _.zone() != start_offset_min) {
                var delta_min = _.zone() - start_offset_min;
                v += delta_min * 60 * 1000;
                _= moment(v).tz(tz);
                start_offset_min = _.zone();

                // Check whether we've backed into the previous timezone again.
                // This can happen during a "spring forward" transition. In this case,
                // it's best to skip this tick altogether (we may be shooting for a
                // non-existent time like the 2AM that's skipped) and go to the next
                // one.
                if (moment(v + p).tz(tz).zone() != start_offset_min) {
                  v += p;
                  _= moment(v).tz(tz);
                  start_offset_min = _.zone();
                }
              }
            
              (a>=Dygraph.DAILY||_.get('hour')%h===0)&&y.push({v:v,label:n(_,a,i,r)});
			        v+=p;
			        _=moment(v).tz(tz);
		        }
	        }
	      }else{
          var start_year = moment(t).tz(tz).year();
          var end_year   = moment(e).tz(tz).year();
          var start_month = moment(t).tz(tz).month();
          
          if(l === 1){
            var step_month = h;
            for (var ii = start_year; ii <= end_year; ii++) {
              for (var j = 0; j < 12;) {
                var dt = moment(new Date(ii, j, 1)).tz(tz); 
                // fix some tz bug
                dt.year(ii);
                dt.month(j);
                dt.date(1);
                dt.hour(0);
                v = dt.valueOf();
                y.push({v:v,label:n(moment(v).tz(tz),a,i,r)});
                j+=step_month;
              }
            }
          }else{
            var step_year = h;
            for (var ii = start_year; ii <= end_year;) {
              var dt = moment(new Date(ii, 1, 1)).tz(tz); 
              // fix some tz bug
              dt.year(ii);
              dt.month(j);
              dt.date(1);
              dt.hour(0);
              v = dt.valueOf();
              y.push({v:v,label:n(moment(v).tz(tz),a,i,r)});
              ii+=step_year;
            }
          }
	      }
	      return y;
	    }else{
       return []; 
	    }
    };
  },

  xAxisLabelFormatterFixedTZ : function(tz){
  
    return function dateAxisFormatter(date, granularity){
      var mmnt = moment(date).tz(tz);
      if (granularity >= Dygraph.DECADAL){
        return mmnt.format('YYYY');
      }else{
        if(granularity >= Dygraph.MONTHLY){
          return mmnt.format('MMM YY');
        }else{
          var frac = mmnt.hour() * 3600 + mmnt.minute() * 60 + mmnt.second() + mmnt.millisecond();
            if (frac === 0 || granularity >= Dygraph.DAILY) {
              return mmnt.format('DD MMM');
            } else {
             if (mmnt.second()) {
               return mmnt.format('HH:mm:ss');
             } else {
               return mmnt.format('HH:mm');
             }
            }
         } 
                        
       }         
   }
  },
         
  xValueFormatterFixedTZ: function(scale, tz) {
                   
    return function(millis) {
      var mmnt = moment(millis).tz(tz);
        if (scale == "yearly")
          return mmnt.format('YYYY') + ' (' + mmnt.zoneAbbr() + ')';
        else if (scale == "monthly" || scale == "quarterly")
          return mmnt.format('MMMM, YYYY')+ ' (' + mmnt.zoneAbbr() + ')';
        else if (scale == "daily" || scale == "weekly")
          return mmnt.format('MMMM, DD, YYYY')+ ' (' + mmnt.zoneAbbr() + ')';
        else
          return mmnt.format('MMMM, DD, YYYY HH:mm:ss')+ ' (' + mmnt.zoneAbbr() + ')';
    }
  },
  
  
  xAxisLabelFormatter : function(){  
    return function(e,a,i){
  	  var r=i("labelsUTC"),
      n=r?Dygraph.DateAccessorsUTC:Dygraph.DateAccessorsLocal,
      o=n.getFullYear(e),
      s=n.getMonth(e),
      l=n.getDate(e),
      h=n.getHours(e),
      p=n.getMinutes(e),
      g=n.getSeconds(e),
      d=n.getSeconds(e);
      
      if(a>=Dygraph.DECADAL)return""+o;
		  if(a>=Dygraph.MONTHLY)return Dygraph.SHORT_MONTH_NAMES_[s]+"&#160;"+o.toString().substring(2);
		
      var u=3600*h+60*p+g+.001*d;
		  return 0===u||a>=Dygraph.DAILY?Dygraph.zeropad(l)+"&#160;"+Dygraph.SHORT_MONTH_NAMES_[s]:Dygraph.hmsString_(h,p,g)
	  }
  },

  xValueFormatter: function(scale) {
    
    var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                      
    return function(millis) {
      var date = new Date(millis);
        if (scale == "yearly")
          return date.getFullYear();
        else if (scale == "monthly" || scale == "quarterly")
          return monthNames[date.getMonth()] + ' ' + date.getFullYear(); 
        else if (scale == "daily" || scale == "weekly")
          return monthNames[date.getMonth()] + ' ' + 
                           date.getDate() + ' ' + 
                           date.getFullYear();
        else
          return date.toLocaleString();
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
  
  addShadingCallback: function(x) {
    
    // bail if no shadings
    if (x.shadings.length == 0)
      return;
    
    // alias this
    var thiz = this;
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing underlayCallback
    var prevUnderlayCallback = attrs["underlayCallback"];
    
    // install callback
    attrs.underlayCallback = function(canvas, area, g) {
      
      // call existing
      if (prevUnderlayCallback)
        prevUnderlayCallback(canvas, area, g);
        
      for (var i = 0; i < x.shadings.length; i++) {
        var shading = x.shadings[i];
        var x1 = thiz.normalizeDateValue(x.scale, shading.from).getTime();
        var x2 = thiz.normalizeDateValue(x.scale, shading.to).getTime();
        var left = g.toDomXCoord(x1);
        var right = g.toDomXCoord(x2);
        canvas.save();
        canvas.fillStyle = shading.color;
        canvas.fillRect(left, area.y, right - left, area.h);
        canvas.restore();
      }
    };
  },
  
  addEventCallback: function(x) {
    
    // bail if no evets
    if (x.events.length == 0)
      return;
    
    // alias this
    var thiz = this;
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing underlayCallback
    var prevUnderlayCallback = attrs["underlayCallback"];
    
    // install callback
    attrs.underlayCallback = function(canvas, area, g) {
      
      // call existing
      if (prevUnderlayCallback)
        prevUnderlayCallback(canvas, area, g);
        
      for (var i = 0; i < x.events.length; i++) {
        
        // get event and x-coordinate
        var event = x.events[i];
        var xPos = thiz.normalizeDateValue(x.scale, event.date).getTime();
        xPos = g.toDomXCoord(xPos);
        
        // draw line
        canvas.save();
        canvas.strokeStyle = event.color;
        thiz.dashedLine(canvas, 
                        xPos, 
                        area.y, 
                        xPos, 
                        area.y + area.h,
                        event.strokePattern);
        canvas.restore();
        
        // draw label
        if (event.label != null) {
          canvas.save();
          thiz.setFontSize(canvas, 12);
          var size = canvas.measureText(event.label);
          var tx = xPos - 4;
          var ty;
          if (event.labelLoc == "top")
            ty = area.y + size.width + 10;
          else
            ty = area.y + area.h - 10;
          canvas.translate(tx,ty);
          canvas.rotate(3 * Math.PI / 2);
          canvas.translate(-tx,-ty);
          canvas.fillText(event.label, tx, ty);
          canvas.restore();
        }
      }
    };
  },
  
  addDateWindowShinyInput: function(id, x) {
      
    // check for an existing drawCallback
    var prevDrawCallback = x.attrs["drawCallback"];
    
    // install the callback
    x.attrs.drawCallback = function(me, initial) {
      
      // call existing
      if (prevDrawCallback)
        prevDrawCallback(me, initial);
        
      // fire input change
      var range = me.xAxisRange();
      var dateWindow = [new Date(range[0]), new Date(range[1])];
      Shiny.onInputChange(id + "_date_window", dateWindow); 
    };
  },
  
  // Add dashed line support to canvas rendering context
  // See: http://stackoverflow.com/questions/4576724/dotted-stroke-in-canvas
  dashedLine: function(canvas, x, y, x2, y2, dashArray) {
    canvas.beginPath();
    if (!dashArray) dashArray=[10,5];
    if (dashLength==0) dashLength = 0.001; // Hack for Safari
    var dashCount = dashArray.length;
    canvas.moveTo(x, y);
    var dx = (x2-x), dy = (y2-y);
    var slope = dx ? dy/dx : 1e15;
    var distRemaining = Math.sqrt( dx*dx + dy*dy );
    var dashIndex=0, draw=true;
    while (distRemaining>=0.1){
      var dashLength = dashArray[dashIndex++%dashCount];
      if (dashLength > distRemaining) dashLength = distRemaining;
      var xStep = Math.sqrt( dashLength*dashLength / (1 + slope*slope) );
      if (dx<0) xStep = -xStep;
      x += xStep
      y += slope*xStep;
      canvas[draw ? 'lineTo' : 'moveTo'](x,y);
      distRemaining -= dashLength;
      draw = !draw;
    }
    canvas.stroke();
  },
  
  setFontSize: function(canvas, size) {
    var cFont = canvas.font;
    var parts = cFont.split(' ');
    if (parts.length === 2)
      canvas.font = size + 'px ' + parts[1];
    else if (parts.length === 3)
      canvas.font = parts[0] + ' ' + size + 'px ' + parts[2];
  },
  
  // Returns the value of a GET variable
  queryVar: function(name) {
    return decodeURI(window.location.search.replace(
      new RegExp("^(?:.*[&\\?]" +
                 encodeURI(name).replace(/[\.\+\*]/g, "\\$&") +
                 "(?:\\=([^&]*))?)?.*$", "i"),
      "$1"));
  },
  
  // We deal exclusively in UTC dates within R, however dygraphs deals 
  // exclusively in the local time zone. Therefore, in order to plot date
  // labels that make sense to the user when we are dealing with days,
  // months or years we need to convert the UTC date value to a local time
  // value that "looks like" the equivilant UTC value. To do this we add the
  // timezone offset to the UTC date.
  normalizeDateValue: function(scale, value) {
    var date = new Date(value); 
    if (scale != "minute" && scale != "hourly") {
      var localAsUTC = date.getTime() + (date.getTimezoneOffset() * 60000);
      date = new Date(localAsUTC);
    }
    return date;
  }
  
});

