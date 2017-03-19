/**
 * @fileoverview Plug-in for hiding/showing time-series.
 */
Dygraph.Plugins.Hide = (function() {

  "use strict";

  /**
   * Create a new instance.
   *
   * @constructor
   */
  var hide = function() {
    this.showSeriesArray = [];
  };

  hide.prototype.toString = function() {
    return 'hide Plugin';
  };

/**
 * This is called during the dygraph constructor, after options have been set
 * but before the data is available.
 * @param {Dygraph} g Graph instance.
 * @return {object.<string, function(ev)>} Mapping of event names to callbacks.
 */
  hide.prototype.activate = function(g) {
    return {
      willDrawChart: this.willDrawChart
    };
  };

  hide.prototype.willDrawChart = function(e) {
    var g = e.dygraph;
    var area = g.plotter_.area;
    var parent = g.graphDiv;

    var visibility = g.visibility();
    for (var i = 0; i < visibility.length; i++) {
      var offset = 20*i+4;
      var cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.checked = true;
      cb.id = i;
      cb.style.position = 'absolute';
      cb.style.top = (area.y + 4) + 'px';
      cb.style.left = (area.x + offset) + 'px';
      cb.style.zIndex = 11;
      this.showSeriesArray.push(true);

      var self = this;

      g.addAndTrackEvent(cb, 'click', function(e) {
        var targetId = e.target.id;
        self.showSeriesArray[targetId] = !self.showSeriesArray[targetId];
        g.setVisibility(targetId, self.showSeriesArray[targetId]);
      });
      
      parent.insertBefore(cb, parent.firstChild);
    }
  };

  return hide;

})();
