# AGENTS.md — AI Assistant Guide for abap-util

> This file follows the cross-tool AGENTS.md convention and is the single
> agent instruction file of this repository — `CLAUDE.md` only points here.

## Project Overview

abap-util provides utility functions for ABAP as class-based methods — strings, JSON/XML/CSV/XLSX, RTTI, messages, logging, locks, calendar, transports, persistence and more. It hides language-version differences between ABAP Cloud and Standard ABAP behind one façade and supports all releases from NW 7.02 to ABAP Cloud.

**License:** MIT
**Language:** English — all code, comments, commit messages, PRs, issues, documentation, and communication must be in English.
**Documentation:** https://abap-util.github.io/docs/

## The Master-Catalog Principle (Superset of All Methods)

This is the most important concept in this repository. Read it before changing anything.

**abap-util is the master catalog for platform-abstraction utilities: it contains all available utility classes with all available methods. Downstream projects do not install it as a dependency — each project decides which classes it needs and embeds a renamed copy of exactly those classes.**

```
abap-util (master catalog, this repo)             Downstream projects (vendored copies)
┌──────────────────────────────┐
│ zabaputil_cl_util_context    │  copy + rename   ┌────────────────────────────────────┐
│  (ALL utility methods,       │ ───────────────→ │ abap2UI5 (src/00/03/):             │
│   full unit test coverage,   │  context class:  │  z2ui5_cl_ui5_util_context (subset)│
│   linted for 7.02/Standard/  │  trim to used    │  z2ui5_cl_ui5_util_http            │
│   Cloud)                     │  methods         │  z2ui5_cx_ui5_util_error           │
│ zabaputil_cl_util_http       │ ───────────────→ ├────────────────────────────────────┤
│ zabaputil_cx_util_error      │  other classes:  │ popups:                            │
│ ...                          │  copy as-is      │  z2ui5_cl_popup_context            │
│                              │                  │  (src/00/, popup subset)           │
└──────────────────────────────┘                  └────────────────────────────────────┘
        ↑                                                          │
        └── periodic AI sync-back: what was added or fixed in ─────┘
            ANY vendored class downstream is merged into abap-util,
            so the master stays the superset of all methods
```

**Why copies instead of a dependency:**
- abapGit has no dependency management — a hard dependency would force installation order and version pinning on every user. Copies keep every consumer "clone and go".
- Namespace isolation: multiple projects (even on different versions of the utils) can coexist in one system without conflicts.
- The context-class copy only carries the methods the project actually uses instead of the full class.

**How the cycle works:**
1. **Class-level selection:** every project decides which utility classes it needs and vendors a renamed copy of exactly those classes.
2. **Method-level trimming — context class only:** the copy of `zabaputil_cl_util_context` is additionally reduced at method level to the methods the project actually uses. Trimming must keep the closure: every private/protected helper a kept method calls (transitively) stays in the copy. All other vendored classes are copied as-is.
3. **New methods are developed locally.** When a project needs a utility method during development that its context-class copy does not have, it is simply written directly into the project's local context class — no upstream round-trip is required. (If the method already exists in this catalog, copy it from here with its helper closure instead of re-implementing it.)
4. **Periodic AI sync-back:** every few weeks an AI compares abap-util with all consumers and merges what was added or fixed downstream into this repository — so abap-util always converges back to the superset of all methods, unit-tested and linted for all targets, and every other consumer can pick them up from here.

   **The sync covers every vendored class, not only the context class.** The context class is where most of the movement is, but a consumer fixes whatever it has a copy of: abap2UI5's `z2ui5_cx_ui5_util_error` and `z2ui5_cl_ui5_util_http` are copies of `zabaputil_cx_util_error` and `zabaputil_cl_util_http` and drift exactly the same way. Diff each consumer's full set of vendored classes against its master here.

   The class names above are abap2UI5's current ones. They carry a `ui5` segment since that repository's rename (`z2ui5_cl_a2ui5_context` → `z2ui5_cl_ui5_util_context`, `z2ui5_cl_a2ui5_http` → `z2ui5_cl_ui5_util_http`, `z2ui5_cx_a2ui5_error` → `z2ui5_cx_ui5_util_error`) — the same three objects under new names, so an older note written under the pre-rename names is describing the same code. A consumer renaming its copy is expected and costs nothing here; read the consumer's own AGENTS.md for the list that is current at sync time.

   **Two things next to a vendored copy are not sync sources.** A consumer may own classes in the same package that have no master here — abap2UI5's `z2ui5_cl_ui5_util_json_fl` is framework-owned and stays downstream. And abap2UI5's `src/99/01/` (`z2ui5_cl_util`, `z2ui5_cl_util_http`, `z2ui5_cx_util_error`, …) holds frozen pre-vendoring copies that receive no fixes; syncing from them would drag dead code back into the master. Both are excluded — check the consumer's own AGENTS.md for its current list.

   **The sync compares method *bodies*, not just method names.** A consumer that fixes a bug in a method it already has produces no missing method at all, so a name-level diff reports "in sync" while the master keeps shipping the broken implementation to every other consumer. Diff each shared method's body (normalizing the renamed class/exception prefixes), and treat a behavioral difference as a sync item exactly like a missing method. Pure formatting and comment-wording differences are expected — each consumer runs its own formatter config — and are not sync items.

   **A method the master has to keep wider than the consumer's is not drift.** A consumer trims its copy, so a symbol its trim dropped will show up as a body difference forever. Known and settled: `class_constructor` keeps `cv_char_util_charsize`, which abap2UI5 dropped because its trimmed copy has no caller for it and the catalogue has two (the XLSX column-width calculation). Also settled: `uuid_get_c32` keeps its `gv_uuid_failed` re-entrancy guard here, which abap2UI5 dropped (2026-10 full sync). Since the two exception classes of this catalog became one, the constructor of `zabaputil_cx_util_error` no longer asks for a uuid either - only the full report does, lazily - so the guard is a safety net now, not a necessity. Record such a case here when it is decided, or the next run re-opens it.

   **Decided in the 2026-11 review (the next sync must not re-open these):**
   - The table lookups of `boolean_check_by_data`, `rtti_check_class_exists`, `rtti_check_class_impl_intf` and `rtti_get_t_attri_by_any` stay `ASSIGN itab[ ... ]` here; abap2UI5 writes them as `READ TABLE ... REFERENCE INTO`, which this repository's lint forbids (`forbidden_statement`). Same behaviour, not a sync item.
   - `xml_srtti_descr` names the S-RTTI mirror by this repository's prefix (`zabaputil_cl_srt_typedescr`); a consumer's own prefix there is a rename, not drift.
   - `zabaputil_cl_util_http=>client_request_cloud` / `client_execute_cloud` call the client, request and response objects by their interface names (`IF_WEB_HTTP_CLIENT~EXECUTE`, `IF_WEB_HTTP_REQUEST~SET_TEXT`, ...). abap2UI5's copy still uses the bare names, which resolve only where the implementing class declares aliases - the master is right, the copy is to be refreshed.
   - `_read_e070` fills `transport` with the request (STRKORR) and `task` with the task (TRKORR), and `bus_tr_add` writes to `task`. Before 2026-11 the two were swapped; a consumer copy that still swaps them is outdated, not ahead.
   - `lang_sap_to_iso` keeps the SAP language key case-sensitive (`c` Catalan, `C` Czech) - never upper-case it.
   - `conv_string_to_number`: with both separators the last one is the decimal separator, one kind several times is a thousands separator, a single separator is the decimal one (`1,000` is 1).
   - A CP row without `*` keeps rendering as the `*low*` token (`filter_get_token_t_by_range_t`), as the test `token_pattern_roundtrip` records - the round trip of such a row is a contains pattern.
   - The BAL methods raise `zabaputil_cx_util_error` when a log cannot be written, deleted or read; "no log matches" stays an empty result.
   - `conv_get_xstring_by_data_uri` reads the header: base64 only with `;base64` (or without any `data:...,` prefix), a percent-encoded payload otherwise (`conv_get_file_by_data_uri`). A consumer copy that base64-decodes every payload is outdated, not ahead.
   - A new row of `zabaputil_cl_util_db` gets an id derived from its handles (SHA-256 via `hash_string`), so two parallel saves of the same handles cannot insert two rows; a random UUID is only the fallback without a digest class. A consumer copy that always draws a UUID is outdated.
   - `itab_distinct`, `itab_group_sum_by` and `itab_count_by` write a value through a string template (`-5`, never `5-`), and `itab_sort_by` is stable (the row index is the last sort key).
   - A `WHILE` whose condition holds a table expression (`line_exists( ... )`) is written as `DO` with an `EXIT`: the 7.02 downport evaluates such a condition once in front of the loop, and the loop never ends.
   - The `<leer>` / `!<leer>` rows of `filter_get_token_range_mapping` are no leftover: the popups' range dialog offers them as "empty" / "not empty". `filter_get_range_by_token( '<leer>' )` reads the token as EQ an empty value.
   - `filter_get_multi_by_sql_where` raises `zabaputil_cx_util_error` for what a filter cannot say (an OR across fields, IS NULL, NOT in front, a subquery, text behind a literal) instead of dropping or rewriting it. A consumer copy that answers with fewer filters is outdated.
   - `bal_create`, `bal_update`, `bal_delete`, `bal_delete_before` and `mail_send` keep `COMMIT WORK` as the default and take `with_commit = abap_false` for callers that must not commit (RAP handlers). On-premise `bal_delete*` search with BAL_DB_SEARCH and delete the headers (BAL_DB_DELETE has no filter parameter); all keys initial deletes nothing.
   - Cloud-only APIs whose methods are interface methods (XCO, the cloud mail and lock APIs) are called through the private `dyn_call`, which finds `IF_...~NAME` on the object's interfaces and types the one importing argument - not by the bare name, which only works where the class declares an alias.

   **Extract methods with their pragmas.** A naive `METHOD <name>.` extractor silently skips `METHOD constructor ##ADT_SUPPRESS_GENERATION.` and every other declaration carrying a pragma or a pseudo-comment — the method then appears in neither the missing list nor the changed list, and a defect in it survives every sync run that has ever been made. The 2026-09 sync found two real changes hiding in exactly that method. Match the pragmas, and sanity-check the extracted method count against `grep -c '^  METHOD'` on both sides.
5. **Multi-environment compatibility is non-negotiable:** every method must work on NW 7.02, Standard ABAP, and ABAP Cloud, because any consumer may run on any of these targets. Environment-specific behavior is branched via `check_abap_cloud( )` and dynamic calls so the code compiles everywhere.

**Why the consumers need this at all:** in every consumer, *all* system- and environment-specific functionality is reached through a method of its context class — never by calling `cl_abap_*`, a function module, or an environment-specific API directly. That is what confines the dependency on SAP standard objects to one class per project and makes those projects portable across all three targets and transpilable to JS. This catalog exists to keep that one class from having to be written from scratch in every project.

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
├── zabaputil_cl_util_xlsx.clas.abap      # XLSX workbooks: internal tables to sheets and back
├── zabaputil_cx_util_error.clas.abap     # THE exception (no-check): text chain, full diagnostic report
├── 00/
│   ├── 01/                               # ajson — DO NOT MODIFY (copied from abap-util/mirror-ajson)
│   └── 02/                               # S-RTTI — DO NOT MODIFY (copied from abap-util/mirror-srtti)
└── pdf/                                  # PDF files in plain ABAP (see "The PDF classes")
```

Every class has a `.testclasses.abap` file. The master carries the **full** test suite — every method, all three targets — and is the only place where a vendored method is guaranteed to be covered. Consumers may additionally test their own copy (abap2UI5 does, for the subset it vendors); that does not replace the coverage here, and a method synced back from a consumer needs its tests written in this repository.

## The PDF classes

`src/pdf/` writes PDF files without Adobe Document Services, spool or forms. Bottom-up: `zabaputil_cl_pdf_writer` (objects, streams, cross-reference table), the resources `zabaputil_if_pdf_font` with `zabaputil_cl_pdf_font_std` / `zabaputil_cl_pdf_font_ttf` and `zabaputil_cl_pdf_image`, `zabaputil_cl_pdf_page` (drawing at fixed positions), `zabaputil_cl_pdf_barcode` (barcodes drawn on a page), `zabaputil_cl_pdf_layout` (wrapping, tables, page breaks) and the façade `zabaputil_cl_pdf`. On top of them `zabaputil_cl_pdf_form` prints a form that is data (`ty_s_form`: elements in the flow or at fixed positions, placeholders for the components of the caller's structure) - what an editor changes and a table stores; `check( form data )` lists what the form names that the data does not have, with the element it is in; the editor itself is an abap2UI5 addon, not part of this catalog. An absolute form (`ty_s_form-absolute`) has no flow: every element stands at its x, y, w, h, a table goes on on the next page, and what stood below it on the canvas follows where it ends (`absolute_elements`). An element with a `condition` is printed only when that component is filled (`!NAME`: empty) - without data always, so the preview shows everything; a hidden element in an absolute form leaves its space empty. A `REPEAT` element prints the elements whose `block` is its name once per row of its source table (flow: one after the other; absolute: every h mm from its y, block elements' y counted from the top of the block, on the next page when a block does not fit); their placeholders are resolved with the row first, then with the data. A text with a height (h > 0) whose lines need more grows the same way, and its lines go on at the top margin of the next page (`absolute_text`); a text without a height never pushes. The data of a print is a structure or an object: `zabaputil_if_pdf_data` marks a class whose public attributes are a form's interface (`fields( )` lists them, `data_sample( )` creates an instance for a preview - only of a class that implements the interface, checked on the class descriptor before anything is created).

- **No SAP API in these classes.** Everything system-specific goes through the context class: `conv_get_text_by_user` (the user's date, time and number format for the `{NAME:date}`, `{NAME:amount:WAERS}`, ... placeholders of a form - a string template, since `CURRENCY =` and `UNIT =` are not in the transpiler, with the decimals from `cur_get_decimals` / `unit_get_decimals`), `conv_get_xstring_by_string` / `conv_get_string_by_xstring`, `zlib_compress` / `zlib_decompress` (CL_ABAP_GZIP), `c_get_codepoints`, and the RTTI helpers `rtti_get_type_kind`, `rtti_get_t_attri_by_any` / `_by_oref`, `rtti_check_class_impl_intf` and `rtti_get_ref_type_name` for the form's data. A consumer vendors the PDF classes as they are, next to its context-class copy with these methods.
- **Tables** (`zabaputil_cl_pdf_layout=>table`, `ty_s_table_options`, in a form `ty_s_element-options`): every option is off by default and the output without options is byte for byte what it was. Sums are added up in a field of the type of the value (`CREATE DATA ... LIKE`), so a sum is written as its values are; only `table( )` adds up, `table_text( )` has texts. A carry-over row is reserved below every data row so it always fits; a row is cut only when it is higher than a fresh page below its header row.
- **Header and footer** are areas (`ty_s_area`: left, center, right, several lines, an image) centred in the top and bottom margin; `set_header( )` / `set_footer( )` are one part of such an area and write what they always wrote. The first page may have areas of its own (`first_page = abap_true`, an empty one for none); a form carries both as data (`header_area`, `footer_area`, `first_page`, `header_first`, `footer_first`).
- **Markup** (`rich_text`, `wrap_markup`, `markup_line`; a form element with `markup = abap_true`): `<b>`, `<i>`, `<u>`, `<color=RRGGBB>`; any other `<` is text. A wrapped line is markup of its own, so the code that handles plain lines (the form's fixed and growing texts) passes it on unchanged. The cuts come from `set_font_styles( )`, or for a standard font from `zabaputil_cl_pdf_font_std=>styled( )`; a TrueType font without them stays upright.
- **Justified text** (`align = 'J'`) writes the words of a line one by one, spread to the width - `Tw` would only widen the single-byte blank of a simple font, not the two-byte one of an embedded TrueType font. The last line of a paragraph stays at the left; in a form's fixed text it is marked by a line feed at its end between `text_lines( )` and `text_draw( )`. `set_keep_lines( )` (widows and orphans, or a paragraph kept whole) and `set_paragraph_spacing( )` are off by default.
- **TrueType subsetting** (`zabaputil_cl_pdf_font_ttf=>subset`, at `write( )`, when every glyph is known): the glyph numbers stay, so `/CIDToGIDMap /Identity` stays; unused glyphs are empty in a rebuilt `glyf`, `loca` becomes the long format, composite glyphs pull in their parts, and only head, hhea, maxp, hmtx, loca, glyf, cvt, fpgm, prep, OS/2, name, post and cmap are kept. Table checksums and head's checkSumAdjustment are computed in two 16-bit halves (`checksum`), since an ABAP integer has no unsigned 32 bits. Checked out of band with fontTools (`checkChecksums=2`) and poppler; a unit test can only read the subset back with this class.
- **OpenType/CFF and collections**: an `OTTO` file is embedded whole as `FontFile3 /Subtype /OpenType` under a `CIDFontType0` without `CIDToGIDMap` (a CFF that is not CID-keyed takes the CIDs as glyph numbers) - not subset, and the header still says `%PDF-1.4` although `/OpenType` is PDF 1.6 (poppler and the common viewers do not mind; PDF/A would). Of a `ttcf` collection the font of `index` is read (its table offsets count from the start of the file) and written as a TrueType file of its own through `subset( all = abap_true )`. Checked out of band with Inter (.otf) and WenQuanYi Zen Hei (.ttc) in poppler.
- **Barcodes** (`zabaputil_cl_pdf_barcode`): the modules are worked out in plain ABAP - Code 128 (sets B and C), EAN-13, QR (byte mode, Reed-Solomon over GF(256) with 0x11D, the eight masks scored by the four penalty rules), Data Matrix ECC 200 (ASCII encodation, square sizes, GF(256) with 0x12D, the placement of ISO/IEC 16022 annex F) - and drawn as one filled rectangle per run of dark modules. The tables (Code 128 patterns, QR blocks and alignment positions) come from python-barcode and segno. The unit tests hold expected modules that were checked out of band: read back from the rendered PDF with zxing-cpp, the QR and Data Matrix symbols with no corrected codeword (`UEC 1.0`). XOR is `BIT-XOR` on `x LENGTH 4`.
- **Bookmarks and links into the document** (`add_bookmark`, `zabaputil_cl_pdf_page=>link_page`, a form text with `bookmark` = its level): both need the numbers of the page objects before the pages are written, so `render( )` reserves them first - only then; a document without either keeps its object numbers. The outline is written from the flat list with levels (a level more than one deeper is one deeper), every entry open.
- **Attachments** (`attach`): an `/EmbeddedFile` stream (media type as a PDF name, size, `ModDate` from `time_get_timestamp`), a file specification with `/F`, `/UF` in UTF-16 and `/AFRelationship`, the catalog's `/Names /EmbeddedFiles` tree (keys sorted) and `/AF` array - what PDF/A-3 and ZUGFeRD ask for. Checked with pypdf: the bytes come back as they went in.
- **Graphics state** (`save`, `restore`, `rotate`, `clip_rect`, `set_alpha`): rotation and clipping last until `restore( )`. Transparency needs an ExtGState resource - the page names one per pair of values (`GS500_500`), `alphas( )` lists them, and `zabaputil_cl_pdf=>render` adds `/ExtGState` to the shared resources only when a page used one, so a document without transparency is unchanged. A form element at a fixed position takes `angle`, `opacity` and (a rectangle) `radius`.
- **Interlaced PNG** (`png_deinterlace`): the seven Adam7 passes are unfiltered pass by pass (`png_unfilter`, all five filters), their pixels put into the rows of the image - bit by bit below eight bits a pixel - and the rows written with filter 0 and packed again; from there it is a plain PNG (predictor 15, alpha split). Plain ABAP over every byte, like the alpha split.
- **Characters outside WinAnsiEncoding**: a standard font with a fallback font (`set_fallback_font`, every standard font of the document, also later ones; a form's `fallback_font`) splits a text into pieces (`zabaputil_cl_pdf_font_std=>parts`) - `zabaputil_cl_pdf_page=>text` writes each piece in its font, `width( )` measures each in its font. Without one they are question marks, as before; `missing_chars( )` reports them, and the missing glyphs of embedded fonts.
- **The file is collected in hex and converted to bytes once** (`zabaputil_cl_pdf_writer=>render`): an xstring has no append operator here, and every offset in the cross-reference table is a byte offset, half the length of the hex text.
- **Text is written as a hex string** (`<48656C6C6F>`), so a content stream is 7-bit ASCII whatever the text is; which bytes a character becomes is the font's business (`zabaputil_if_pdf_font~encode`).
- **A parameter that takes a length is typed `numeric`**, a text `clike`: callers pass integers, packed numbers and floats, and a fully typed by-reference parameter would refuse all but one of them on a real system. A text literal is no number - pass `'0.5'` through a typed constant or variable.
- **The tests compare operators, not pixels.** To look at a result, transpile a throwaway class that returns `render( )` and open the file - `pdftoppm`, `pdffonts` and `pdfimages` (poppler) show what a viewer makes of it.

## Build & Validation

```bash
npm ci
npm run lint                  # Lint, Standard ABAP (v750)
npm run lint:cloud            # Lint, ABAP Cloud
npm run rename                # Rename check: zabaputil_ -> znamespace_ (the longest prefix allowed), 0 issues
npm run auto_transpile        # Transpile ABAP → JS
npm run unit                  # Run unit tests in Node.js
npm test                      # all of the above
npm run downport              # rewrites src/ to 7.02 syntax - never commit the result
```

CI runs on every pull request and on every push to `main`: the lints for Standard ABAP and ABAP Cloud (`ABAP_STANDARD.yaml`, `ABAP_CLOUD.yaml`), the rename check (`test_rename.yaml`) - consumers vendor renamed copies, so every class must still lint under another prefix - and the transpiled unit tests **twice** (`test_unit.yaml`): on the source as `main` ships it and on the same source after the downport, linted at v702. `auto_downport.yaml` pushes that downport to the branch `702` on every push to `main`, `ABAP_702.yaml` lints the branch. All must stay green — they guarantee that any subset of methods can be vendored into any consumer on any release.

Every action in `.github/workflows/` is pinned to a commit sha with the tag as a comment; `bump-deps.yaml` moves the pins and the npm dependencies once a month, in one pull request.

## XLSX: one class, the system behind the context class

`zabaputil_cl_util_xlsx` writes and reads `.xlsx` workbooks and names no SAP class itself: ZIP (`zip_pack`, `zip_unpack`), UTF-8 (`conv_get_xstring_by_string`), the XML reader (`xml_reader_create`, `xml_reader_next` - the sXML reader, token by token, no tree), escaping (`c_escape_xml`), RTTI (`rtti_get_t_attri_by_any`, `rtti_get_type_kind`, the `cv_typedescr_typekind_*` constants, `rtti_create_tab_by_names`) and the data element texts all come from `zabaputil_cl_util_context`. Keep it that way - it is what makes the class run unchanged on 7.02, Standard ABAP, ABAP Cloud and the transpiled runtime, and what a consumer copies along when it vendors the class.

- **No regex, no XSLT, no DDIC object.** Format codes, cell addresses and numbers are read character by character.
- **A sheet is rendered in `add_sheet( )`**, not kept as a reference to the caller's table - the table may be gone by `get_xlsx( )`.
- **A formula is written without a value, or with one worked out here** (the totals row adds up while the rows are written): `get_xlsx( )` then asks Excel to recalculate on load (`calcPr fullCalcOnLoad`). A totals formula is `SUBTOTAL( )` - over the column of the Excel table by name, over the cells where the sheet has no table.
- **Reading goes row by row** (`sheet_read_row`) and never builds the whole sheet in memory for `get_table( )`.
- **`xml_reader_create` sets no option on the sXML reader**: as it comes it reports a text of blanks only where it is all an element holds and leaves out the indentation between elements, and the transpiled runtime implements no `set_option`.
- **The reading tests are slow in the transpiled runtime by its XML parser** (it searches the document from the start for every token) - keep the workbooks of the tests small; a system parses in the kernel.
- `zabaputil_cl_util_context=>conv_get_xlsx_by_itab` stays as the one-sheet writer of a trimmed context copy; `conv_get_itab_by_xlsx` stays a stub that raises, because the context class cannot call the class that calls it.

## One class per purpose

There is exactly one HTTP class (`zabaputil_cl_util_http`) and one exception class (`zabaputil_cx_util_error`). Until 2026-11 each had a twin (`zabaputil_cl_http`, a subset; `zabaputil_cx_error`, the superset with the chain rendering and the full report) and the consumers copied different ones. They were merged under the `util_` names with the superset's code. Do not add a second class for a purpose that has one - widen the one that is there.

## Checking a consumer's copies: `sync-check`

```bash
node .github/scripts/sync-check.mjs --consumer ../business-api            # exit 1 on a finding
node .github/scripts/sync-check.mjs --consumer ../business-api --report   # list only
```

A consumer describes its copies in `.github/abap-util.json`: the master commit they were taken from, the rename (copy name → master name) and, per copy, the master file, whether it is trimmed, and the differences that are decided (`settled`) or have no master (`own`), each with its reason. The method list is not written down - it is read from the copy. The script compares bodies and declarations after the rename, without comments and layout, and extracts methods with their pragmas (the two traps described above). It reports `changed`, `copy-only` (sync it back) and, for a copy that is not trimmed, `missing`.

Run it at the manifest's commit to see what the consumer changed since the copy was taken, and at `main` to see what a refresh would bring. It is the first step of every sync run; the decision what to merge stays with the reader.

The consumers with a manifest - each sync run goes through all of them:

| Consumer | Check in its CI |
| --- | --- |
| business-api | yes (`test.yaml`, job `sync`) |
| abap2UI5, popups, range-filter, table-content-loader | no - run it here at sync time |
| layout-management, lock-manager, se16n, sql-console, table-maintenance | no - run it here at sync time (manifests added 2026-11) |

A consumer that vendors a class of this catalog without a manifest is compared by hand until it gets one; add it to the table when it does.

## Rules for AI Assistants

1. **Do not modify `src/00/01/` (ajson) and `src/00/02/` (S-RTTI)** — copied by `vendor-mirror.yaml` from this organization's mirror repositories ([mirror-ajson](https://github.com/abap-util/mirror-ajson), [mirror-srtti](https://github.com/abap-util/mirror-srtti)), which rename the upstream projects to `zabaputil_` and apply their local patches. A change belongs there (upstream, or the mirror's `.github/scripts/patch.mjs`); abap2UI5 vendors from the same two mirrors and renames on its side.
2. **Never break the master-catalog contract** (see above): this repository must remain the superset of all utility methods across all consumers — for *every* vendored class, not only the context class. Methods added downstream are merged back here by the periodic AI sync, and unit tests live here.
3. **Always run `npx abaplint`** before considering changes complete.
4. **Multi-environment compatibility** — code must work on NW 7.02, Standard ABAP, and ABAP Cloud. No direct use of on-premise-only or cloud-only APIs without a dynamic-call branch.
5. **String literals use backticks** (`` ` ``), not single quotes; `xsdbool()` for booleans; `NEW #()` instead of `CREATE OBJECT`.
6. **Public API stability:** downstream copies mirror method signatures, so default to additive changes — new methods and new optional parameters, not edits to what exists.

   A rename is possible, because nobody installs this repository as a dependency (every consumer runs its own renamed copy, so a rename here breaks no running system). But it only pays off if it lands everywhere at once: rename in this catalog **and** in every consumer's context class in the same coordinated change, or the next sync reports the signature as drift and reverts it. Before renaming, check each consumer for callers that pass the parameter **by name** — a consumer that cannot be edited blocks the rename outright. Concrete case: `rtti_create_sel_tab_type`'s `ir_tab` keeps its Hungarian name because abap2UI5's frozen `src/99` passes it as a named argument; the parameter is documented as such at the declaration in both repositories.
