#!/bin/sh
# Regenerate docs/showcase/table.png from the fake data in render.el.
# Needs emacs (30.1+), git, and rsvg-convert (librsvg).
set -eu
cd "$(dirname "$0")/../.."
make deps >/dev/null
${EMACS:-emacs} -Q --batch -L . -L .deps/vui.el -l docs/showcase/render.el docs/showcase/table.svg
rsvg-convert -z 1.5 docs/showcase/table.svg -o docs/showcase/table.png
rm -f docs/showcase/table.svg
ls -l docs/showcase/table.png
