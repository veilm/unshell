echo "script-1=$1"
echo "script-2=$2"
echo "script-count=$#"
printf "script-star=%s|\n" $*
printf "script-quoted=%s|\n" "$*"
