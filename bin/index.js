#!/usr/bin/env node
try {
  require("../bundle/Main/index.js");
} catch (e) {
  require("../output/Main/index.js").main();
}
