#!/usr/bin/env python3
import glob
import json
import os
import re
import sys

RANGE_RE = re.compile(r"^(\d+)-(\d+)$")

def expand_brace(token: str):
    if '{' not in token or '}' not in token:
        return [token]
    start = token.find('{')
    end = token.find('}', start + 1)
    if end == -1:
        return [token]
    prefix = token[:start]
    suffix = token[end + 1:]
    inner = token[start + 1:end]
    parts = inner.split(',') if inner else ['']
    expanded = []
    for part in parts:
        match = RANGE_RE.match(part)
        if not match:
            expanded.append(f"{prefix}{part}{suffix}")
            continue
        start_text, end_text = match.group(1), match.group(2)
        start_num = int(start_text)
        end_num = int(end_text)
        step = 1 if start_num <= end_num else -1
        width = 0
        if (start_text.startswith("0") and len(start_text) > 1) or (
            end_text.startswith("0") and len(end_text) > 1
        ):
            width = max(len(start_text), len(end_text))
        for value in range(start_num, end_num + step, step):
            if width:
                expanded.append(f"{prefix}{value:0{width}d}{suffix}")
            else:
                expanded.append(f"{prefix}{value}{suffix}")
    return expanded


def expand_glob(token: str):
    if not any(ch in token for ch in "*?["):
        return [token]
    matches = glob.glob(token)
    if not matches:
        return [token]
    return sorted(matches)

def expand_tilde(token: str):
    if token == "~" or token.startswith("~/"):
        return os.path.expanduser(token)
    return token


def main():
    if len(sys.argv) < 2:
        print(json.dumps([]))
        return 0
    token = expand_tilde(sys.argv[-1])
    expanded = []
    for item in expand_brace(token):
        expanded.extend(expand_glob(item))
    print(json.dumps(expanded, ensure_ascii=False))
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
