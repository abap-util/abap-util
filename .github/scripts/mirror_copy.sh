#!/usr/bin/env bash
set -euo pipefail

# Copies the renamed abap2UI5 sources (abaplint --rename output) into src/,
# mapping the upstream packages to the abap-util package layout:
#
#   upstream src/00/01  (ajson)            -> src/00/01
#   upstream src/00/02  (S-RTTI)           -> src/00/02
#   upstream src/99/01  (utility classes)  -> src/00/03
#   upstream src/00/03  (core classes)     -> src/00/04
OUT=".github/abaplint/output$(pwd)/input/abap2UI5/src"

rm -rf src
mkdir -p src/00
cp "$OUT/00/package.devc.xml" src/00/
cp -r "$OUT/00/01" src/00/01
cp -r "$OUT/00/02" src/00/02
cp -r "$OUT/99/01" src/00/03
cp -r "$OUT/00/03" src/00/04

# package descriptions of the upstream packages refer to abap2UI5 - adjust them for abap-util
sed -i 's|<CTEXT>abap2UI5 - libs (external)</CTEXT>|<CTEXT>abap-util</CTEXT>|' src/00/package.devc.xml
sed -i 's|<CTEXT>abap2UI5 - obsolete - utilities</CTEXT>|<CTEXT>abap-util - utilities</CTEXT>|' src/00/03/package.devc.xml
sed -i 's|<CTEXT>abap-util - https://github.com/oblomov-dev/abap-util</CTEXT>|<CTEXT>abap-util - core</CTEXT>|' src/00/04/package.devc.xml
