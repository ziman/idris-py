#!/bin/bash

die() {
    echo "$1" >&2
    exit 1
}

run() {
    prog="$1"

    idris -p python --codegen python "$1".idr -o "$1".py \
        || die "could not compile $1"

    python "$1".py \
        || die "could not compile $1"
}

cd examples/
set -x

for fname in *.idr; do
    run "${fname%.idr}"
done
