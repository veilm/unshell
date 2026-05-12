#!/usr/bin/env python3
import json
import os
import sys


def directory_candidates(fragment: str):
    if fragment.endswith("/"):
        directory = fragment
        prefix = ""
    else:
        directory, prefix = os.path.split(fragment)
        if directory:
            directory = directory + "/"

    scan_dir = directory if directory else "."
    try:
        names = os.listdir(scan_dir)
    except OSError:
        return []

    include_hidden = prefix.startswith(".") or "/." in fragment or fragment.startswith(".")
    out = []
    for name in names:
        if not include_hidden and name.startswith("."):
            continue
        if prefix and not name.startswith(prefix):
            continue
        path = os.path.join(scan_dir, name)
        if not os.path.isdir(path):
            continue
        out.append(f"{directory}{name}/")
    return sorted(out, key=lambda item: (item.lower(), item))


def main():
    fragment = os.environ.get("USH_COMP_WORD", "")
    if sys.argv[1:] == ["rmdir"]:
        print(json.dumps(directory_candidates(fragment)))
        return 0

    print(json.dumps([]))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
