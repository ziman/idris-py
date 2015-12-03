#!/bin/bash

die() {
    echo "!! $1, aborting" >&2
    exit 1
}

make-deterministic() {
    prog="$1"

    case "$prog" in
        example)
            sed -r 's/thread [AB] done/thread A or B done/'
            ;;

        *)  cat  # no changes
            ;;
    esac
}

run() {
    prog="$1"
    temp="$(tempfile)"

    echo "$1"

    echo "  compiling..."
    idris -p python --codegen python examples/"$1".idr -o examples/"$1".py \
        || die "could not compile $1"

    expected=examples/"$1".expected.txt
    echo "  running..."
    if [ "$update_expected" = 1 ]; then
        { python examples/"$1".py \
            || die "could not run $1"
        }   | make-deterministic "$1" \
            > "$expected"
    else
        { python examples/"$1".py \
            || die "could not run $1"
        }   | make-deterministic "$1" \
            > "$temp"

        diff "$temp" "$expected" \
            || die "unexpected output (see: vimdiff $temp $expected)"

        rm "$temp"
    fi
}

if [ "$1" = "update-expected" ]; then
    update_expected=1
else
    update_expected=0
fi

(cd examples; ls -1 *.idr) | while read fname; do
    run "${fname%.idr}"
done
