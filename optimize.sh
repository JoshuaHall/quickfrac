#!/bin/sh

set -e

js="dist/elm.js"
min="dist/elm.min.js"

elm-format src/ --yes

elm make --optimize --output=$js src/Main.elm

npx google-closure-compiler -O ADVANCED -W quiet $js --js_output_file $min

echo "Compiled size:$(wc $js -c) bytes  ($js)"
echo "Minified size:$(wc $min -c) bytes  ($min)"
echo "Gzipped size: $(gzip $min -c | wc -c) bytes"