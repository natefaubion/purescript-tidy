import process from "process";

export function hrtime() {
  var t = process.hrtime()
  return { seconds: t[0], nanos: t[1] };
}

export function hrtimeDiff(old) {
  return function() {
    var t = process.hrtime([old.seconds, old.nanos]);
    return { seconds: t[0], nanos: t[1] };
  };
}
