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
    this.popup = null; // Popup div
  };

  var isNumeric = function(v) {
    var obj = {};
    return (typeof v === 'number' ||
      obj.toString.call(v) === '[object Number]') && !isNaN(v);
  };

  var normalizeDateValue = function(scale, value, fixedtz) {
    var date = new Date(value);
    if (scale !== 'minute' &&
      scale !== 'hourly' &&
      scale !== 'seconds' &&
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

    this.popup = this.initPopup();

    return {
      didDrawChart: this.didDrawChart,
      clearChart: this.clearChart,
      select: this.select,
      deselect: this.deselect,
    };
  };

  arrow.prototype.initPopup = function() {
    var div = document.createElement('div');
    div.className = 'arrow-popup arrow-popup--hidden';
    this.dygraph.graphDiv.appendChild(div);
    return div;
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
    var i;

    for (i = 0; i < this.data.length; i++) {
      var a = this.data[i];
      var xval = isNumeric(a.xval) ?
        a.xval : normalizeDateValue(a.scale, a.xval, a.fixedtz).getTime();
      arrows[xval + ',' + a.series] = a;
    }

    this.arrowPoints = [];

    for (i = 0; i < this.dygraph.layout_.points.length; i++) {
      var points = this.dygraph.layout_.points[i];
      for (var j = 0; j < points.length; j++) {
        var p = points[j];
        var k = p.xval + ',' + p.name;
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
        this.calcPosition(canvas,
                          point.canvasx,
                          point.canvasy,
                          tickHeight);
    this.setCanvasPosition(canvas, position);

    this.dygraph.graphDiv.appendChild(canvas);
    this.arrows.push(canvas);
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
      'left': 270,
      'right': 90,
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

  arrow.prototype.select = function(e) {
    for (var i = 0; i < e.selectedPoints.length; i++) {
      var p = e.selectedPoints[i];
      if (!p.hasOwnProperty('arrow')) {
        this.hidePopup();
        continue;
      }
      this.showPopup(p);
    }
  };

  arrow.prototype.showPopup = function(p) {
    this.popup.innerText = p.arrow.text;
    this.setPopupPosition(p);

    this.popup.classList.remove("arrow-popup--hidden");
    for (var i = 0; i < this.popup.classList.length; i++) {
      var oldClass = this.popup.classList.item(i);
      var newClass = "arrow-popup--direction-" + p.arrow.direction;
      if (oldClass === newClass) {
        break;
      }
      if (oldClass.startsWith("arrow-popup--direction-")) {
        this.popup.classList.replace(oldClass, newClass);
        break;
      }
      this.popup.classList.add(newClass);
    }
  };

  arrow.prototype.setPopupPosition = function(p) {
    var popupWidth = this.popup.offsetWidth;
    var popupHeight = this.popup.offsetHeight;
    var offset = 10;
    var leftPopup, topPopup;

    if (p.arrow.direction === "up") {
      leftPopup = p.canvasx - popupWidth / 2;
      topPopup = p.canvasy - popupHeight - offset;
    } else if (p.arrow.direction === "down") {
      leftPopup = p.canvasx - popupWidth / 2;
      topPopup = p.canvasy + offset;
    } else if (p.arrow.direction === "left") {
      leftPopup = p.canvasx - popupWidth - offset;
      topPopup = p.canvasy - popupHeight / 2;
    } else if (p.arrow.direction === "right") {
      leftPopup = p.canvasx + offset;
      topPopup = p.canvasy - popupHeight / 2;
    } else if (p.arrow.direction === "se") {
      leftPopup = p.canvasx + offset;
      topPopup = p.canvasy + offset;
    } else if (p.arrow.direction === "sw") {
      leftPopup = p.canvasx - popupWidth - offset;
      topPopup = p.canvasy + offset;
    } else if (p.arrow.direction === "ne") {
      leftPopup = p.canvasx + offset;
      topPopup = p.canvasy - popupHeight - offset;
    } else if (p.arrow.direction === "nw") {
      leftPopup = p.canvasx - popupWidth - offset;
      topPopup = p.canvasy - popupHeight - offset;
    }

    // Strip redundant CSS classes
    this.popup.classList.remove("arrow-popup--left-bound", "arrow-popup--right-bound");

    var xLabelWidth = this.dygraph.getOptionForAxis('axisLabelWidth', 'y');
    if (leftPopup < xLabelWidth) {
      leftPopup = xLabelWidth;
      this.popup.classList.add("arrow-popup--left-bound");
    }

    var area = this.dygraph.plotter_.area;
    var diff = (leftPopup + popupWidth) - area.w - xLabelWidth - offset;
    if (diff > 0) {
      leftPopup = leftPopup - diff;
      this.popup.classList.add("arrow-popup--right-bound");
    }

    this.popup.style.left = leftPopup + "px";
    this.popup.style.top = topPopup + "px";
  };

  arrow.prototype.hidePopup = function() {
    this.popup.style.left = "";
    this.popup.style.top = "";
    this.popup.classList.add("arrow-popup--hidden");
  };

  arrow.prototype.deselect = function(e) {
    this.hidePopup();
  };

  return arrow;
})();
