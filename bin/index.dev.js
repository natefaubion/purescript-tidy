#!/usr/bin/env -S node --experimental-json-modules
import url from "url";
import { main } from "../output/Main/index.js";

process.env["TIDY_INSTALL_LOC"] = url.fileURLToPath(new URL('..', import.meta.url));

main();
