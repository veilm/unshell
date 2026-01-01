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

hook global WinSetOption filetype=unshell %{
    require-module unshell
    set-option window static_words %opt{unshell_static_words}
    hook -once -always window WinSetOption filetype=.* %{ remove-hooks window unshell-.+ }
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
add-highlighter shared/unshell/code/keyword regex %{\b(if|else|elif|for|each|foreach|in|def|function|fn)\b} 0:keyword
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
    declare-option str-list unshell_static_words if else elif for foreach in def function fn alias unalias cd set export local return exit builtin eval
}

# add-highlighter shared/unshell/code/keyword regex \b(if|else|elif|for|foreach|in|def|function|fn)\b 0:keyword
add-highlighter shared/unshell/code/builtin regex %{\b(alias|unalias|cd|set|export|local|return|exit|builtin|eval)\b} 0:builtin

@
