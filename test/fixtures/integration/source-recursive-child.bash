printf "argv:%s|%s\n" "$1" "$2"
prefix=child
export CHILD_MARK=seen
sh -c 'exit 7'
