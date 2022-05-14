#!/usr/bin/env node
import path from "path";
import { main } from "../bundle/Main/index.js";

const __dirname = path.dirname(new URL(import.meta.url).pathname);
process.env["TIDY_INSTALL_LOC"] = path.resolve(__dirname, "..");

main();
