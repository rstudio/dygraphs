/**
 * (Trade) Arrows Dygraph Plugin.
 */
Dygraph.Plugins.Arrow = (function() {
  'use strict';

  var arrow = function(data) {
    this.arrowPoints = []; // Dygraph points with arrows
    this.arrows = []; // Arrows DOM elements attached to chart
    this.data = data; // Raw arrow data
    this.dygraph = null; // Dygraph reference
  };

  var isNumeric = function(v) {
    var obj = {};
    return (typeof v === 'number' ||
      obj.toString.call(v) === '[object Number]') && !isNaN(v);
  };

  var normalizeDateValue = function(scale, value, fixedtz) {
    var date = new Date(value);
    if (scale != 'minute' &&
      scale != 'hourly' &&
      scale != 'seconds' &&
      !fixedtz) {
      var localAsUTC = date.getTime() + (date.getTimezoneOffset() * 60000);
      date = new Date(localAsUTC);
    }
    return date;
  };

  arrow.prototype.toString = function() {
    return 'Arrow Plugin';
  };

  arrow.prototype.activate = function(g) {
    this.dygraph = g;

    return {
      didDrawChart: this.didDrawChart,
      clearChart: this.clearChart
    };
  };

  arrow.prototype.didDrawChart = function() {
    // Early out in the (common) case of no data.
    if (this.data.length === 0) return;

    this.attachArrowsToChart();

    var canvasx;
    for (var i = 0; i < this.data.length; i++) {
      var item = this.data[i];
      if (isNumeric(item.xval)) {
        canvasx = this.dygraph.toDomXCoord(item.xval);
      } else {
        canvasx = normalizeDateValue(item.scale, item.xval, item.fixedtz)
          .getTime();
        canvasx = this.dygraph.toDomXCoord(canvasx);
      }
      console.log(canvasx);
    }
  };

  arrow.prototype.clearChart = function() {
    // Early out in the (common) case of zero arrows.
    if (this.arrows.length === 0) return;

    this.detachArrows();
  };

  arrow.prototype.attachArrowsToChart = function() {
    this.evaluateArrows();

    for (var i = 0; i < this.arrowPoints.length; i++) {
      var point = this.arrowPoints[i];
      if (this.pointInvisible(point)) continue;

      this.attachArrow(point);
    }
  };

  arrow.prototype.evaluateArrows = function() {
    var arrows = {};

    for (var i = 0; i < this.data.length; i++) {
      var a = this.data[i];
      var xval = isNumeric(a.xval) ?
        a.xval : normalizeDateValue(a.scale, a.xval, a.fixedtz).getTime();
      arrows[xval + ',' + a.series] = a;
    }
    console.log(arrows);

    this.arrowPoints = [];

    for (var i = 0; i < this.dygraph.layout_.points.length; i++) {
      var points = this.dygraph.layout_.points[i];
      for (var j = 0; j < points.length; j++) {
        var p = points[j];
        var k = p.xval + ',' + p.name;
        console.log(k);
        if (k in arrows) {
          p.arrow = arrows[k];
          this.arrowPoints.push(p);
        }
      }
    }
  };

  arrow.prototype.pointInvisible = function(point) {
    var area = this.dygraph.plotter_.area;

    if (point.canvasx < area.x || point.canvasx > area.x + area.w ||
      point.canvasy < area.y || point.canvasy > area.y + area.h) {
      return true;
    }
    return false;
  };

  arrow.prototype.attachArrow = function(point) {
    var tickHeight = 10;

    var canvas = this.makeCanvas(point.arrow, tickHeight);
    var position =
        this.calcPosition(
                          canvas,
                          point.canvasx,
                          point.canvasy,
                          tickHeight);
    this.setCanvasPosition(canvas, position);

    var div = this.makeDiv(canvas, 'test');
    this.setDivPosition(div, position);

    this.dygraph.graphDiv.appendChild(canvas);
    this.dygraph.graphDiv.appendChild(div);
    this.arrows.push(canvas);
    this.arrows.push(div);
  };

  arrow.prototype.makeCanvas = function(arrow, tickHeight) {
    var size = 11;
    var width = (size + tickHeight) * 4;
    var height = width;
    var canvas = document.createElement('canvas');
    var ctx = canvas.getContext('2d');
    canvas.width = width;
    canvas.height = height;
    canvas.style.width = width + 'px'; // for IE
    canvas.style.height = height + 'px'; // for IE
    canvas.style.position = 'absolute';
    canvas.style.pointerEvents = 'none';

    var cx = width / 2;
    var cy = width / 2;
    this.rotateCanvas(ctx, arrow.direction, tickHeight);
    ctx.translate(-cx, -cy + tickHeight);

    this.shape(ctx, size, arrow.strokeColor, arrow.fillColor);

    return canvas;
  };

  arrow.prototype.calcPosition = function(canvas, x, y, tickHeight) {
    return {
      'left': Math.ceil(x - canvas.width / 2),
      'top': y - canvas.height / 2 + tickHeight
    };
  };

  arrow.prototype.setCanvasPosition = function(canvas, position) {
    canvas.style.left = position.left + 'px';
    canvas.style.top = position.top + 'px';
  };

  arrow.prototype.rotateCanvas = function(ctx, direction, tickHeight) {
    var directions = {
      'up': 0,
      'down': 180,
      'left': 90,
      'right': 270,
      'ne': 45,
      'se': 135,
      'sw': 225,
      'nw': 315
    };
    var rotation = directions[direction] || 0;
    var cx = ctx.canvas.width / 2;
    var cy = cx;
    ctx.translate(cx, cy - tickHeight);
    ctx.rotate(Math.PI / 180 * rotation);
  };

  arrow.prototype.getBoundingBox = function(ctx, alpha) {
    alpha = alpha || 15;

    var minX = Infinity;
    var minY = Infinity;
    var maxX = -Infinity;
    var maxY = -Infinity;

    var w = ctx.canvas.width;
    var h = ctx.canvas.height;

    var data = ctx.getImageData(0, 0, w, h).data;

    for (var x = 0; x < w; ++x) {
      for (var y = 0; y < h; ++y) {
        var a = data[(w * y + x) * 4 + 3];

        if (a > alpha) {
          if (x > maxX) maxX = x;
          if (x < minX) minX = x;
          if (y > maxY) maxY = y;
          if (y < minY) minY = y;
        }
      }
    }
    return {
      x: minX,
      y: minY,
      w: maxX - minX,
      h: maxY - minY
    };
  };

  arrow.prototype.makeDiv = function(canvas, tooltip) {
    var ctx = canvas.getContext('2d');
    var bbox = this.getBoundingBox(ctx);
    var div = document.createElement('div');

    div.style.position = 'absolute';
    div.style.width = bbox.w + 'px';
    div.style.height = bbox.h + 'px';
    div.style.left = bbox.x + 'px';
    div.style.top = bbox.y + 'px';
    div.title = tooltip;

    return div;
  };

  arrow.prototype.setDivPosition = function(div, position) {
    var currentLeft = parseInt(div.style.left, 10);
    var currentTop = parseInt(div.style.top, 10);
    div.style.left = currentLeft + position.left + 'px';
    div.style.top = currentTop + position.top + 'px';
  };

  arrow.prototype.shape = function(ctx, size, stroke, fill) {
    var cx = ctx.canvas.width / 2;
    var cy = cx;

    ctx.strokeStyle = stroke;
    ctx.fillStyle = fill;
    ctx.lineWidth = 0.6;

    ctx.beginPath();
    ctx.moveTo(cx, cy);
    ctx.lineTo(cx + size / 2, cy + size);
    ctx.lineTo(cx + size / 6, cy + size * 0.8);
    ctx.lineTo(cx + size / 6, cy + size * 2);
    ctx.lineTo(cx - size / 6, cy + size * 2);
    ctx.lineTo(cx - size / 6, cy + size * 0.8);
    ctx.lineTo(cx - size / 2, cy + size);
    ctx.lineTo(cx, cy);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();
  };

  arrow.prototype.detachArrows = function() {
    for (var i = 0; i < this.arrows.length; i++) {
      var a = this.arrows[i];
      if (a.parentNode) {
        a.parentNode.removeChild(a);
      }
      this.arrows[i] = null;
    }
    this.arrows = [];
  };

  return arrow;
})();
