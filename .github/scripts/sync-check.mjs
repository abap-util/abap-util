#!/usr/bin/env node
/*
 * sync-check.mjs - compares a consumer's vendored copies with their masters in
 * this catalog. It reads the consumer's manifest (.github/abap-util.json) and
 * reports, per copy, what differs from the master checkout it runs in.
 *
 *   node .github/scripts/sync-check.mjs --consumer <dir> [--manifest <file>]
 *                                       [--report] [--list]
 *
 * The manifest, in the consumer:
 *
 *   {
 *     "master": "abap-util/abap-util",
 *     "commit": "<sha of the master commit the copies were taken from>",
 *     "rename": [["zabaputil_cl_bus_util_", "zabaputil_cl_util_"]],
 *     "copies": [
 *       { "copy": "src/00/zabaputil_cl_bus_util_context.clas.abap",
 *         "master": "src/zabaputil_cl_util_context.clas.abap",
 *         "trimmed": true,
 *         "settled": { "<method>": "<why this difference stays>" },
 *         "own": { "<method>": "<why this method has no master>" } }
 *     ]
 *   }
 *
 * "rename" maps the copy's names back to the master's, pair by pair, in order
 * (case-insensitive). "trimmed": true says the copy carries a subset of the
 * methods - a method the master has and the copy has not is then no finding.
 * The method list of a copy is not written down: it is read from the copy.
 *
 * Compared are the method bodies and the method declarations, after the
 * rename, without comments and with whitespace collapsed - each consumer runs
 * its own formatter, so layout and comment wording are no findings. Code
 * outside literals is compared case-insensitively.
 *
 * Findings:
 *   changed   - body or declaration differs from the master
 *   copy-only - the copy has a method the master has not (sync it back)
 *   missing   - only for a copy that is not trimmed: the master has a method
 *               the copy has not
 *
 * Run against the master at the manifest's commit, a finding means the copy
 * was edited after it was taken: sync it back, or record it under "settled".
 * Run against the master's main (--report: always exit 0), the findings are
 * what a refresh of the copy would bring.
 */
import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const args = process.argv.slice(2);
const opt = (name) =>
  args.includes(name) ? args[args.indexOf(name) + 1] : undefined;
const consumer = opt("--consumer");
if (!consumer) {
  console.error(
    "usage: sync-check.mjs --consumer <dir> [--manifest <file>] [--report] [--list]",
  );
  process.exit(2);
}
const consumerDir = resolve(consumer);
const manifestFile = resolve(
  consumerDir,
  opt("--manifest") ?? ".github/abap-util.json",
);
if (!existsSync(manifestFile)) {
  console.error(`sync-check: no manifest at ${manifestFile}`);
  process.exit(2);
}
const manifest = JSON.parse(readFileSync(manifestFile, "utf8"));

// ---------------------------------------------------------------- ABAP
// Splits a line into code and literals, drops the comment. Literals keep
// their case, code is lowercased.
function stripLine(line) {
  if (line.startsWith("*")) return "";
  let out = "";
  let i = 0;
  while (i < line.length) {
    const c = line[i];
    if (c === '"') break;
    if (c === "'" || c === "`" || c === "|") {
      let j = i + 1;
      while (j < line.length) {
        if (c === "|" && line[j] === "\\") {
          j += 2;
          continue;
        }
        if (line[j] === c) {
          if (c !== "|" && line[j + 1] === c) {
            j += 2;
            continue;
          }
          break;
        }
        j++;
      }
      out += line.slice(i, j + 1);
      i = j + 1;
      continue;
    }
    out += c.toLowerCase();
    i++;
  }
  return out;
}

function normalize(text, rename) {
  let s = text
    .split("\n")
    .map(stripLine)
    .join(" ")
    .replace(/\s+/g, " ")
    // pragmas and the "!" escape of a parameter name carry no behaviour
    .replace(/##\w+/g, "")
    .replace(/!(?=\w)/g, "")
    .replace(/\s+/g, " ")
    .replace(/\s*([().,:=>])\s*/g, "$1")
    .trim();
  for (const [from, to] of rename)
    s = s.replace(
      new RegExp(from.replace(/[.*+?^${}()|[\]\\/]/g, "\\$&"), "gi"),
      to.toLowerCase(),
    );
  return s;
}

function parse(file, rename) {
  const text = readFileSync(file, "utf8").replace(/^﻿/, "").replace(/\r/g, "");
  const lines = text.split("\n");
  const impl = lines.findIndex((l) => /^\s*CLASS\s+\S+\s+IMPLEMENTATION/i.test(l));
  const bodies = new Map();
  const decls = new Map();

  // bodies: METHOD <name> [pragmas]. ... ENDMETHOD.
  for (let i = impl < 0 ? lines.length : impl; i < lines.length; i++) {
    const m = lines[i].match(/^\s*METHOD\s+([\w~/]+)[^.]*\./i);
    if (!m) continue;
    let j = i + 1;
    while (j < lines.length && !/^\s*ENDMETHOD\s*\./i.test(lines[j])) j++;
    bodies.set(
      m[1].toLowerCase(),
      normalize(lines.slice(i + 1, j).join("\n"), rename),
    );
    i = j;
  }

  // declarations: [CLASS-]METHODS <name> ... .   (one statement each)
  const end = impl < 0 ? lines.length : impl;
  for (let i = 0; i < end; i++) {
    const m = lines[i].match(/^\s*(?:CLASS-)?METHODS\s+([\w~/]+)/i);
    if (!m) continue;
    let j = i;
    let stmt = "";
    for (; j < end; j++) {
      const code = stripLine(lines[j]);
      stmt += code + "\n";
      if (/\.\s*$/.test(code)) break;
    }
    decls.set(m[1].toLowerCase(), normalize(stmt, rename));
    i = j;
  }
  return { bodies, decls };
}

// ---------------------------------------------------------------- check
let findings = 0;
const rename = manifest.rename ?? [];
console.log(
  `sync-check: ${manifest.master ?? "master"} @ ${manifest.commit ?? "(no commit)"} -> ${consumerDir}`,
);
for (const c of manifest.copies ?? []) {
  const copyFile = join(consumerDir, c.copy);
  const masterFile = join(ROOT, c.master);
  for (const f of [copyFile, masterFile])
    if (!existsSync(f)) {
      console.error(`sync-check: ${f} does not exist`);
      process.exit(2);
    }
  const copy = parse(copyFile, rename);
  const master = parse(masterFile, []);
  const settled = c.settled ?? {};
  const own = c.own ?? {};
  const changed = [];
  const copyOnly = [];
  const missing = [];
  const noted = [];
  let same = 0;
  for (const [name, body] of copy.bodies) {
    if (!master.bodies.has(name)) {
      (Object.hasOwn(own, name) ? noted : copyOnly).push(
        Object.hasOwn(own, name) ? `${name} - own: ${own[name]}` : name,
      );
      continue;
    }
    const what = [];
    if (body !== master.bodies.get(name)) what.push("body");
    if (
      copy.decls.has(name) &&
      master.decls.has(name) &&
      copy.decls.get(name) !== master.decls.get(name)
    )
      what.push("declaration");
    if (!what.length) same++;
    else if (Object.hasOwn(settled, name)) noted.push(`${name} - settled: ${settled[name]}`);
    else changed.push(`${name} (${what.join(", ")})`);
  }
  if (!c.trimmed)
    for (const name of master.bodies.keys())
      if (!copy.bodies.has(name)) missing.push(name);
  for (const name of [...Object.keys(settled), ...Object.keys(own)])
    if (!copy.bodies.has(name))
      changed.push(`${name} (named in the manifest, not in the copy)`);

  console.log(`\n${c.copy}\n  master: ${c.master}${c.trimmed ? " (trimmed)" : ""}`);
  console.log(
    `  ${copy.bodies.size} methods: ${same} identical, ${changed.length} changed, ${copyOnly.length} copy-only, ${missing.length} missing, ${noted.length} noted`,
  );
  for (const n of changed) console.log(`  changed:   ${n}`);
  for (const n of copyOnly) console.log(`  copy-only: ${n}`);
  for (const n of missing) console.log(`  missing:   ${n}`);
  for (const n of noted) console.log(`  noted:     ${n}`);
  if (args.includes("--list"))
    console.log(`  methods:   ${[...copy.bodies.keys()].sort().join(" ")}`);
  findings += changed.length + copyOnly.length + missing.length;
}
console.log(`\nsync-check: ${findings} finding(s)`);
process.exit(findings && !args.includes("--report") ? 1 : 0);
