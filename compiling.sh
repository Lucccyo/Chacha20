#!/bin/sh
N="$1"
shift
ocamlopt "./$N.ml" -o "./$N" && ./"$N" "$@"
