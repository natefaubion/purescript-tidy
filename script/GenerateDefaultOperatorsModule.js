const fs = require("fs");
const os = require("os");
const path = require("path");

exports.tmpdir = (prefix) => () =>
  fs.mkdtempSync(path.join(os.tmpdir(), prefix), "utf-8");
