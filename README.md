# unshell

a minimal shell. concise syntax, very little bloat while staying extensible

## tour: how unshell differs from POSIX shells

- tab-indented or brace-delimited control blocks (`if`/`elif`/`else`/`for`/`foreach`)
	```bash
	# no : needed. it's more concise than python that's right
	if true
		pwd
		date
		echo this is a branch
	elif [ -d /tmp/ ] { echo not true } # or use braces if you'd like
	elif false {
		# multiline
	}
	else
		sudo poweroff
	```
- atomic arguments by default: `$var` expands to exactly one argument (no implicit splitting or globbing)
	```bash
	# creates "./my file.txt", not ./my and ./file.txt
	var="my file.txt"
	touch $var
	```
- explicit re-tokenization with `...`: opt into POSIX-like splitting by re-parsing a string
	```bash
	# runs `seq 5 | tac`, not `seq 5 "| tac"` or `seq 5 "|" "tac"`
	ending="| tac"
	seq 5 ...$ending
	```
- square-brackets as a shorthand for subshells
	```bash
	# equivalent to echo $(pwd). [] is much faster to type than $()
	echo [seq 10 | tail -1]
		# => 10

	# all of these will be one argument, not expanding by default
	# unless you use ...[] or ...$() etc

	echo "[pwd]" # [] are literals when inside quotes, to avoid collision
		# => [pwd]
	echo "$(pwd) $[pwd]" # $() and $[] are still subshells inside quotes
		# => /tmp /tmp
	echo '$(pwd)' # everything is literal inside single quotes
		# => $(pwd)
	```
- stream-based, line-based `foreach`. like xargs except not cancer to use
	```bash
	# idk if this should be the default, just add it to your config
	# if you like it
	alias each foreach

	# indent-based block
	seq 5 | each i
		touch $i.txt
		echo [date +%s] "done text file creation for index $i"

	# use braces if you want to chain further
	ls | each file { du -h $file } | sort -h
	```
- arbitrary shorthand string handling, using your userspace handlers
	```bash
	cd /tmp/
	touch foo1.txt foo2.txt foo3.txt

	# prints a literal foo*.txt. you have no handler by default
	echo foo*.txt

	# enable the shipped extension handler. easy to modify or replace
	set expansions.handler util/expansion_handler.py

	# intercept tokens containing *, outside of strings
	set expansions.characters "*" on

	# prints foo1.txt foo2.txt foo3.txt, as 3 separate arguments
	# expansion_handler.py received "foo*.txt"
	# and returned a JSON array ["foo1.txt", "foo2.txt", "foo3.txt"]
	echo foo*.txt

	# you can easily make your own handler for syntax like
	# fu@3 -> "fufufu", or anything else you want
	# Gemini 3's ideas: s3://bucket/* and pkg@latest
	```

more info: [full spec](./docs/spec.md)

repl details: [repl guide](./docs/repl.md)

## install

deps: Rust, Python (optional; for the default config), fzf (optional; for better repl tab completion)

```bash
git clone https://github.com/veilm/unshell
./unshell/install.sh
```

if you don't need interactive usage and want to skip the
[rustyline](https://github.com/kkawakam/rustyline) pull by cargo, you can
install unshell without the repl:
```bash
./unshell/install.sh --no-repl
```

## usage

```bash
ush                 # repl
ush path/to/script  # run a script file line-by-line
ush --norc          # skip startup files
ush --rc path       # source only this init file
```

## support

open a GitHub issue or ping me on twitter. I'd be happy to answer any possible question, or discuss Unix or shells in general

## license

MIT
