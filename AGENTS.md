# AGENTS.md — AI Assistant Guide for abap-util

> This file follows the cross-tool AGENTS.md convention and is the single
> agent instruction file of this repository — there is no separate
> `CLAUDE.md`; Claude Code reads `AGENTS.md` natively.

## Project Overview

abap-util provides utility functions for ABAP as class-based methods — strings, JSON/XML/CSV/XLSX, RTTI, messages, logging, locks, calendar, transports, persistence and more. It hides language-version differences between ABAP Cloud and Standard ABAP behind one façade and supports all releases from NW 7.02 to ABAP Cloud.

**License:** MIT
**Language:** English — all code, comments, commit messages, PRs, issues, documentation, and communication must be in English.
**Documentation:** https://abap-util.github.io/docs/

## The Master/Copy Principle (Single Source of Truth)

This is the most important concept in this repository. Read it before changing anything.

**abap-util is the master repository for platform-abstraction utilities. Downstream projects do not install it as a dependency — they embed a renamed, trimmed copy of the classes they need.**

```
abap-util (master, this repo)                     Downstream projects (vendored copies)
┌──────────────────────────────┐
│ zabaputil_cl_util_context    │  copy + rename   ┌────────────────────────────────────┐
│  (all utility methods,       │ ───────────────→ │ abap2UI5:                          │
│   full unit test coverage,   │  trim to used    │  z2ui5_cl_abap2ui5_context         │
│   linted for 7.02/Standard/  │  methods         │  (src/00/03/, framework subset)    │
│   Cloud)                     │                  ├────────────────────────────────────┤
│ zabaputil_cl_util_http       │ ───────────────→ │ popups:                            │
│ zabaputil_cx_error           │                  │  z2ui5_cl_popup_context            │
│ ...                          │                  │  (src/00/, popup subset)           │
└──────────────────────────────┘                  └────────────────────────────────────┘
```

**Why copies instead of a dependency:**
- abapGit has no dependency management — a hard dependency would force installation order and version pinning on every user. Copies keep every consumer "clone and go".
- Namespace isolation: multiple projects (even on different versions of the utils) can coexist in one system without conflicts.
- Each consumer only carries the methods it actually uses instead of the full class.

**Rules:**
1. **All development happens here first.** Bug fixes and new functionality are implemented and unit-tested in this repository, then propagated to the downstream copies. Never accept a change that only exists in a copy.
2. **A copy may differ from the master in exactly two ways:** the class name (project namespace, e.g. `z2ui5_cl_abap2ui5_context`) and the set of methods (trimmed to what the project uses). Method implementations must stay textually identical to the master.
3. **Trimming must include the closure:** when a public method is copied, every private/protected helper it calls (transitively) must be copied with it.
4. **When a consumer needs a method that is not in its copy yet,** copy it (with its helper closure) from the current master state here — do not re-implement it downstream.
5. **Multi-environment compatibility is non-negotiable:** every method must work on NW 7.02, Standard ABAP, and ABAP Cloud, because any consumer may run on any of these targets. Environment-specific behavior is branched via `context_check_abap_cloud( )` and dynamic calls so the code compiles everywhere.

## Repository Structure

```
src/
├── zabaputil_cl_util_context.clas.abap   # THE master utility façade (strings, JSON, XML, RTTI,
│                                         #   UUID, calendar, locks, BAL, transports, ...)
├── zabaputil_cl_util_http.clas.abap      # Unified HTTP abstraction (on-premise + cloud)
├── zabaputil_cl_util_db.clas.abap        # Key/value persistence (table zabaputil_t_91)
├── zabaputil_cl_util_log.clas.abap       # Logging
├── zabaputil_cl_util_msg.clas.abap       # Message handling
├── zabaputil_cl_util_range.clas.abap     # Select-option range builders
├── zabaputil_cl_util_xml.clas.abap       # Fluent XML builder
├── zabaputil_cl_http.clas.abap           # HTTP helper
├── zabaputil_cx_error.clas.abap          # No-check exception
├── zabaputil_cx_util_error.clas.abap     # Exception (compatibility)
└── 00/
    ├── 01/                               # ajson mirror — DO NOT MODIFY (synced from upstream)
    └── 02/                               # S-RTTI mirror — DO NOT MODIFY (synced from upstream)
```

Every class has a `.testclasses.abap` file — unit tests live in the master, not in the downstream copies.

## Build & Validation

```bash
npm install
npx abaplint                  # Lint (v750 syntax, downport rule for 7.02 compatibility)
npm run auto_transpile        # Transpile ABAP → JS
npm run unit                  # Run unit tests in Node.js
```

CI lints against all three targets (`ABAP_702.yaml`, `ABAP_STANDARD.yaml`, `ABAP_CLOUD.yaml`) and runs the transpiled unit tests (`test_unit.yaml`). All must stay green — they guarantee that any subset of methods can be vendored into any consumer on any release.

## Rules for AI Assistants

1. **Do not modify `src/00/01/` (ajson) and `src/00/02/` (S-RTTI)** — mirrored from external projects.
2. **Never break the master/copy contract** (see above): changes land here first, copies stay textually identical per method, tests live here.
3. **Always run `npx abaplint`** before considering changes complete.
4. **Multi-environment compatibility** — code must work on NW 7.02, Standard ABAP, and ABAP Cloud. No direct use of on-premise-only or cloud-only APIs without a dynamic-call branch.
5. **String literals use backticks** (`` ` ``), not single quotes; `xsdbool()` for booleans; `NEW #()` instead of `CREATE OBJECT`.
6. **Public API stability:** downstream copies mirror method signatures — never change or remove existing public methods, parameters, or constants. Additive changes only.
