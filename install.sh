#!/bin/sh
set -e

usage() {
    echo "usage: $0 [--no-repl]"
}

repl=on

for arg in "$@"; do
    case "$arg" in
        --no-repl)
            repl=off
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "unknown option: $arg"
            usage
            exit 1
            ;;
    esac
done

cd "$(dirname "$0")"

make REPL="$repl"
sudo make REPL="$repl" install
