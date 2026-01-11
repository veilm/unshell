# unshell (ush)
# -----------------------------------------------

# Detection
# ---------

hook global BufCreate .*\.ush %{
    set-option buffer filetype unshell
}

hook global BufCreate .* %{
    evaluate-commands %sh{
        if head -n 1 "$kak_buffile" | rg -q '^#![[:space:]]*/usr/bin/env[[:space:]]+ush'; then
            if [ -z "$kak_opt_filetype" ] || [ "$kak_opt_filetype" = "sh" ]; then
                printf "set-option buffer filetype unshell\n"
            fi
        fi
    }
}

# Initialization
# -------------

define-command -hidden unshell-trim-indent %<
    evaluate-commands -no-hooks -draft -itersel %!
        execute-keys x
        try %! execute-keys -draft s \h+$ <ret> d !
    !
>

define-command -hidden unshell-indent-on-new-line %<
    evaluate-commands -draft -itersel %<
        # preserve previous line indent
        try %! execute-keys -draft <semicolon> K <a-&> !
        # cleanup trailing whitespace from previous line
        try %! execute-keys -draft k : unshell-trim-indent <ret> !
        # indent after line ending with {
        try %! execute-keys -draft , k x <a-k> \{\h*$ <ret> j i<tab> !
        # indent after block keywords without inline {
        try %!
            execute-keys -draft , k x <a-k> ^\h*(if|elif|else|for|foreach|each|while|def|function|fn)\b[^{]*$ <ret> j i<tab>
        !
    >
>

define-command -hidden unshell-open-line-below %{
    execute-keys -with-hooks o
    unshell-indent-on-new-line
}

define-command -hidden unshell-open-line-above %{
    execute-keys -with-hooks O
    unshell-indent-on-new-line
}

hook global WinSetOption filetype=unshell %{
    require-module unshell
    set-option window static_words %opt{unshell_static_words}
    hook window InsertChar \n -group unshell-indent unshell-indent-on-new-line
    hook window ModeChange pop:insert:.* -group unshell-trim-indent %{ try %{ execute-keys -draft <semicolon> x s ^\h+$ <ret> d } }
    map -docstring "open line below with indent" window normal o ":unshell-open-line-below<ret>"
    map -docstring "open line above with indent" window normal O ":unshell-open-line-above<ret>"
    hook -once -always window WinSetOption filetype=.* %{
        remove-hooks window unshell-.+
        try %{ unmap window normal o }
        try %{ unmap window normal O }
    }
}

hook -group unshell-highlight global WinSetOption filetype=unshell %{
    try %{ remove-highlighter window/unshell }
    add-highlighter window/unshell ref unshell
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/unshell }
}

provide-module unshell %@
# Highlighters
# ------------

add-highlighter shared/unshell regions
add-highlighter shared/unshell/code default-region group
add-highlighter shared/unshell/code/keyword regex %{\b(if|else|elif|for|each|foreach|while|in|def|function|fn)\b} 0:keyword
add-highlighter shared/unshell/double_string region %{(?<!\\)(?:\\\\)*\K"} %{(?<!\\)(?:\\\\)*"} group
add-highlighter shared/unshell/single_string region %{(?<!\\)(?:\\\\)*\K'} %{'} fill string
add-highlighter shared/unshell/comment region (?<!\\)(?:\\\\)*(?:^|\h)\K# '$' fill comment

add-highlighter shared/unshell/double_string/fill fill string
add-highlighter shared/unshell/double_string/expansion regex %{(?<!\\)(?:\\\\)*\K\$(\w+|#|\?|\$|\*|\d+)} 0:value
add-highlighter shared/unshell/double_string/capture regex (?<!\\)(?:\\\\)*\K\$\[[^\]\n]*\] 0:function
add-highlighter shared/unshell/double_string/command_subst regex (?<!\\)(?:\\\\)*\K\$\([^)\n]*\) 0:function

add-highlighter shared/unshell/code/variable regex %{(?<!\\)(?:\\\\)*\K\$(\w+|#|\?|\$|\*|\d+)} 0:value
add-highlighter shared/unshell/code/capture regex (?<!\\)\[(?!\h)[^\]\n]*\] 0:function
add-highlighter shared/unshell/code/test_brackets regex (?<!\\)\[\h+[^\]\n]*\h+\] 0:operator
add-highlighter shared/unshell/code/command_subst regex (?<!\\)\$\([^)\n]*\) 0:function
add-highlighter shared/unshell/code/spread regex \.\.\. 0:operator
add-highlighter shared/unshell/code/assignment regex ((?<![-:])\b\w+)= 1:variable
add-highlighter shared/unshell/code/operators regex %{(&&|\|\||\||;)} 0:operator

try %{
    declare-option str-list unshell_static_words if else elif for foreach each while in def function fn alias unalias cd set export local return exit builtin eval break continue
}

# add-highlighter shared/unshell/code/keyword regex \b(if|else|elif|for|foreach|in|def|function|fn)\b 0:keyword
add-highlighter shared/unshell/code/builtin regex %{\b(alias|unalias|cd|set|export|local|return|exit|builtin|eval|break|continue)\b} 0:builtin

@
