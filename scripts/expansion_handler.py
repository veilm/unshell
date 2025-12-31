#!/usr/bin/env python3
import json
import sys

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
    return [f"{prefix}{part}{suffix}" for part in parts]


def main():
    if len(sys.argv) < 2:
        print(json.dumps([]))
        return 0
    token = sys.argv[-1]
    expanded = expand_brace(token)
    print(json.dumps(expanded))
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
