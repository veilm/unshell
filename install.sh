#!/bin/sh
set -e

usage() {
    echo "usage: $0 [--no-repl]"
}

repl=on

if ! command -v cargo >/dev/null 2>&1; then
    echo "error: cargo not found. install Rust from https://rustup.rs/ first."
    exit 1
fi

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
config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
if [ -n "$config_home" ] && [ -d "$config_home/kak/autoload/filetype" ]; then
    install -m 644 util/unshell.kak "$config_home/kak/autoload/filetype/unshell.kak"
elif [ -n "$config_home" ] && [ -d "$config_home/kak" ]; then
    install -m 644 util/unshell.kak "$config_home/kak/unshell.kak"
fi
sudo make REPL="$repl" install
