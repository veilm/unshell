# unshell

Prototype implementation of the unshell (`ush`) binary.

## Building

```bash
make build
```

## Installing

Build as your user first, then install (often with `sudo`) so Cargo doesn't need elevated permissions.

```bash
sudo make install             # installs to /usr/local/bin/ush
DESTDIR="$HOME/.local" make install  # user-local install without sudo
```

## Usage

```bash
./target/release/ush                 # interactive prompt (temporary)
./target/release/ush path/to/script  # run a script file line-by-line
```
