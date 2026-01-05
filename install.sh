#!/bin/sh
set -e

usage() {
    echo "usage: $0 [--no-repl]"
}

if ! command -v cargo >/dev/null 2>&1; then
    echo "error: cargo not found. install Rust from https://rustup.rs/ first."
    exit 1
fi

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

build_flags=""
if [ "$repl" = "off" ]; then
    build_flags="--no-default-features"
fi

cargo build --release $build_flags

destdir="${DESTDIR:-}"
prefix="${PREFIX:-/usr/local}"
bin_dir="${destdir}${prefix}/bin"
init_dir="${destdir}/etc/unshell"

mkdir -p "$bin_dir"
install -m755 util/expansion_handler.py "$bin_dir/ush-expansion-handler"
install -m755 util/quote "$bin_dir/ush-quote"

dest="$bin_dir/ush"
if [ -f "$dest" ]; then
    if cmp -s target/release/ush "$dest"; then
        echo "ush unchanged; skipping install."
    else
        install -m755 target/release/ush "$dest"
    fi
else
    install -m755 target/release/ush "$dest"
fi

if [ ! -f "$init_dir/init" ]; then
    mkdir -p "$init_dir"
    install -m644 util/unshell_init "$init_dir/init"
fi

config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
if [ -n "$config_home" ] && [ -d "$config_home/kak/autoload/filetype" ]; then
    install -m 644 util/unshell.kak "$config_home/kak/autoload/filetype/unshell.kak"
elif [ -n "$config_home" ] && [ -d "$config_home/kak" ]; then
    install -m 644 util/unshell.kak "$config_home/kak/unshell.kak"
fi
