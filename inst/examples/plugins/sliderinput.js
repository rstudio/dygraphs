/**
 * @license
 * Copyright 2016 Jeffrey Owen Hanson (jeffrey.hanson@uqconnect.edu.au)
 * MIT-licensed (http://opensource.org/licenses/)
 */

/*global Dygraph:false */
/*jshint globalstrict: true */
Dygraph.Plugins.SliderInput = (function() {
  "use strict";

  /**
   * Creates the sliderinput
   *
   * @constructor
   */

  var sliderinput = function(opt_options) {
    /* create widgets */
    this.canvas_ = document.createElement("canvas"); // canvas to draw bars on
    this.button_ = null
    
    /* pass arguments from R */
    opt_options = opt_options || {};
    this.strokeStyle_ = opt_options.strokeStyle || null;
    this.strokePattern_ = opt_options.strokePattern || null;
    this.animate_ = opt_options.animate || null;
    
    /* set default parameters */
    this.bar_point_ = null; // point associated with bar, if not drawn then null
    this.closest_point_ = null; // closest point to cursor
    this.over_ = false; // true when mouse is over the canvas
    this.animated_ = false; // true when the graph is in an animated state
    this.timer_ = null; // timer object used for animation
    this.graph_height_= null; // graph height when mouse is over graph
    this.graph_width_= null; // graph width when mouse is over graph
    this.ignore_next_click_ = false; // should next click be ignored?
  };

  sliderinput.prototype.toString = function() {
    return "SliderInput Plugin";
  };

  /**
   * @param {Dygraph} g Graph instance.
   * @return {object.<string, function(ev)>} Mapping of event names to callbacks.
   */
  sliderinput.prototype.activate = function(g) {
    /* initialise canvas */
    g.graphDiv.appendChild(this.canvas_);
    
    /* return methods */
    return {
      willDrawChart: this.willDrawChart,
      didDrawChart: this.didDrawChart,
      select: this.select
    };
  };

  sliderinput.prototype.willDrawChart = function (e) {
    /* initialise */
    var g = e.dygraph;
    
    // short-circuit: skip redeclaring all this stuff if we've already been over it
    if (this.button_ !== null) {
      var showButton = (this.animate_ !== null) && this.over_;
      this.show(showButton);
      return;
    }

    // define click event
    this.click = function(point) {
      /* simulate the user clicking on a point in the graph */
      if (point !== null) {
        Shiny.onInputChange(g.maindiv_.id + "_click", {
          date: g.shinyValueFormatter(point.xval),
          x_closest_point: g.shinyValueFormatter(point.xval),
          y_closest_point: point.yval,
          '.nonce': Math.random() // Force reactivity if click hasn't changed
        })
      } else {
        Shiny.onInputChange(g.maindiv_.id + "_click", {
          date: 'NA',
          x_closest_point: 'NA',
          y_closest_point: 'NA',
          '.nonce': Math.random() // Force reactivity if click hasn't changed
        })
      }
    };

    // define function to determine if any points inside graph
    this.anyPointsInsideGraph = function() {
      var date_range = g.xAxisRange();
      var no_points_inside_range = (date_range[0] > g.layout_.points[0][0].xval) && (date_range[1] < g.layout_.points[0][1].xval) && (g.layout_.points[0].length == 2);
      return(!no_points_inside_range);
    };
    
    // define function to get the first point inside a graph
    this.getFirstPoint = function() {
      var counter = 0;
      var point = g.layout_.points[0][counter];
      var date_range = g.xAxisRange();
      while (point.xval < date_range[0]) {
        counter++;
        point = g.layout_.points[0][counter];
      }
      return point;
    }
    
    // define function to get the last point inside a graph
    this.getLastPoint = function() {
      var counter = g.layout_.points[0].length-1;
      var point = g.layout_.points[0][counter];
      var date_range = g.xAxisRange();
      while (point.xval > date_range[1]) {
        counter--;
        point = g.layout_.points[0][counter];
      }
      return point;
    }
    
    /* animation button */
    // create the button
    this.button_ = document.createElement('button');
    this.button_.innerHTML = this.animate_.playButton;
    this.button_.style.display = 'none';
    this.button_.style.position = 'absolute';
    var area = g.plotter_.area;
    this.button_.style.top = (area.y + 10) + 'px';
    this.button_.style.left = (area.x + 30) + 'px';
    this.button_.style.zIndex = 11;
    var parent = g.graphDiv;
    var main = g.maindiv_;
    main.insertBefore(this.button_, main.firstChild);

    // add event hadling to the button
    var self = this;
    this.button_.onclick = (function() {
      /* function definitions */
      
      // move slider to first point
      var resetSliderToStart = function() {
        if (self.anyPointsInsideGraph()) {
          var new_point = self.getFirstPoint();
          self.clear_bars(); // clear bars
          self.add_bar(new_point); // add new bar
          self.click(new_point); // click on point
        } else {
          self.stop_animation();
        }
      };

      // can slider to the next point?
      var canStepNext = function() {
        if (self.anyPointsInsideGraph()) {
          return(self.bar_point_.idx < self.getLastPoint().idx);
        } else {
          self.stop_animation();
        }
      };

      // move slider to the next point
      var stepSliderToNext = function() {
        if (self.anyPointsInsideGraph()) {
          var new_point = g.layout_.points[0][(self.bar_point_.idx - g.layout_.points[0][0].idx) + 1];
          self.clear_bars(); // clear bars
          self.add_bar(new_point); // add new bar
          self.click(new_point); // click on point
        } else {
          self.stop_animation();
        }
      };
     
      var start_animation = function() {
        /* check that there are points on the graph, and if there are none then exit */
        if (!self.anyPointsInsideGraph()) {
          return;
        }
        
        /* start animation */
        // if no bar selected, then select first point in window
        self.animated_ = true;
        self.button_.innerHTML = self.animate_.pauseButton;
        if (self.bar_point_ == null) {
          resetSliderToStart();
        }
        
        // main animation function
        var animation_workhorse = function() {
          if (self.animate_.loop && !canStepNext()) {
            resetSliderToStart();
          } else {
              stepSliderToNext()
              if (!self.animate_.loop && !canStepNext()) {
                self.stop_animation();
              }
          }
        };
        
        // start animation
        self.timer_ = setInterval(animation_workhorse, self.animate_.interval);
      };
      
      self.stop_animation = function() {
        /* stop animation */
        self.animated_ = false;
        self.button_.innerHTML = self.animate_.playButton;
        clearTimeout(self.timer_);
      };
      
      /* main operations */
      // swap state
      self.animated_ = !self.animated_
      // start/stop animation
      if (self.animated_) {
        start_animation();
      } else {
        self.stop_animation();
      }
    });
    g.addAndTrackEvent(main, 'mouseover', function() {
      /* show start/stop buttons */
      self.show(true);
      self.over_ = true;
    });
    g.addAndTrackEvent(main, 'mouseout', function() {
      /* hide start/stop buttons */
      self.show(false);
      self.over_ = false;
    });
    g.addAndTrackEvent(parent, 'click', function() {
      /* check if click should be ignored */
      if (self.ignore_next_click_) {
        self.ignore_next_click_ = false;
        return;
      }
      /* draw new bar */
      if (self.animated_) {
        self.stop_animation(); // stop animation if animated
      }
      self.clear_bars(); // clear bars
      if (self.anyPointsInsideGraph()) {
        self.add_bar(self.closest_point_); // add new line if closest point with graph range
        // click is handled automatically by dygraphs in-build event handler
      }
    });
  };
  
  sliderinput.prototype.didDrawChart = function(e) {    
    /* initialise */
    var g = e.dygraph;
    /* move bar when zooming in or out*/
    if ((this.bar_point_ !== null)) {
      // draw new bar if any points inside plotting region      
      if (this.anyPointsInsideGraph()) {
        if ((this.bar_point_.idx >= g.boundaryIds_[0][0]) && (this.bar_point_.idx <= g.boundaryIds_[0][1])) {
          // redraw bar for point
          var new_point = g.layout_.points[0][(this.bar_point_.idx - g.layout_.points[0][0].idx)];
          this.clear_bars(); // clear bars
          this.add_bar(new_point); // add new bar
          // do not simulate clicking a point since the same date-time will be returned as the previous
        } else if (this.bar_point_.idx < g.boundaryIds_[0][0]) {
          // draw bar on left side of plotting region
          var new_point = g.getFirstPoint();
          this.clear_bars(); // clear bars
          this.add_bar(new_point); // add new bar
          this.click(new_point); // click on point
        } else {
          // draw bar on right side of plotting region
          var new_point = g.getLastPoint();
          this.clear_bars(); // clear bars
          this.add_bar(new_point); // add new bar
          this.click(new_point); // click on point
        }
      } else {
        this.clear_bars(); // clear bars
        this.click(null); // return NA to indicate that no data is shown
      }
      
      // ignore next click caused by zooming
      this.ignore_next_click_ = true;
      
    }
  };
  
  sliderinput.prototype.add_bar = function(point) {
    /* add bar to canvas */
    // extract values from point 
    var canvas_position = Math.floor(point.canvasx) + 0.5;
    // set up canvas
    var width = this.graph_width_;
    var height = this.graph_height_;
    this.canvas_.width = width;
    this.canvas_.height = height;
    this.canvas_.style.width = width + "px";    // for IE
    this.canvas_.style.height = height + "px";  // for IE
    // draw bar on canvas
    var ctx = this.canvas_.getContext("2d");
    ctx.strokeStyle = "rgba("+this.strokeStyle_+")";
    ctx.setLineDash(this.strokePattern_);
    ctx.beginPath();
    ctx.moveTo(canvas_position, 0);
    ctx.lineTo(canvas_position, height);
    ctx.stroke();
    ctx.closePath();
    this.bar_point_ = point;
  };

  sliderinput.prototype.clear_bars = function() {
    /* clear all bars from graph */
    // remove points
    var ctx = this.canvas_.getContext("2d");
    ctx.clearRect(0, 0, this.canvas_.width, this.canvas_.height);
    this.bar_point_ = null;
  };
    
  sliderinput.prototype.show = function(enabled) {
    /* show animation buttons */
    this.button_.style.display = enabled ? '' : 'none';
  };

  sliderinput.prototype.select = function(e) {
    /* set bar variables */
    this.graph_height_ = e.dygraph.height_;
    this.graph_width_ = e.dygraph.width_;
    this.closest_point_ = e.dygraph.selPoints_[0];
  };

  sliderinput.prototype.destroy = function() {
    this.button_.parentElement.removeChild(this.button_);
    this.ignore_next_click = null;
    this.bar_point_ = null; 
    this.over_ = null; 
    this.timer_ = null;
    this.animated_ = null;
    this.canvas_ = null;
    this.x_closest_point_ = null;
    this.graph_height_ = null;
    this.graph_width_ = null;
  };

  return sliderinput;
  
})();
