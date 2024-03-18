import fs from "fs";
import os from "os";
import path from "path";

export function tmpdir(prefix) {
  return () =>
    fs.mkdtempSync(path.join(os.tmpdir(), prefix), "utf-8");
}
