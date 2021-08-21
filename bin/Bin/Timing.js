const process = require("process");

exports.hrtime = function() {
  var t = process.hrtime()
  return { seconds: t[0], nanos: t[1] };
};

exports.hrtimeDiff = function(old) {
  return function() {
    var t = process.hrtime([old.seconds, old.nanos]);
    return { seconds: t[0], nanos: t[1] };
  };
};
