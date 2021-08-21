#!/usr/bin/env node
var path = require("path");
process.env["TIDY_INSTALL_LOC"] = path.resolve(__dirname, "..");
try {
  require("../bundle/Main/index.js");
} catch (e) {
  require("../output/Main/index.js").main();
}
