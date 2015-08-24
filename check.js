var fs = require('fs');
var path = fs.workingDirectory;
var listReducer = function (p, c) {
  var re = /\.html$/;
  if (re.test(c)) {
    p.push(path + fs.separator + c);
  }
  return p;
};
var list = fs.list(path).reduce(listReducer, []);
var page = require('webpage').create();
var loadInProgress = false;
var pageIndex = 0;

var interval = setInterval(function () {
  if (!loadInProgress && pageIndex < list.length) {
    page.open(list[pageIndex]);
  }
  if (pageIndex == list.length) {
    phantom.exit();
  }
}, 250);

page.onLoadStarted = function () {
  loadInProgress = true;
};

page.onLoadFinished = function () {
  loadInProgress = false;
  pageIndex++;
}
page.onError = function (msg, trace) {
  var msgStack = [
    'URL: ' + page.url,
    'ERROR: ' + msg
  ];
  msgStack.push('TRACE:');
  trace.forEach(function (t) {
    msgStack.push('\t' + t.file + ': ' + t.line + (t.function ? ' (in function "' + t.function +'")' : ''));
  });
  console.error(msgStack.join('\n'));
  phantom.exit(1);
}
