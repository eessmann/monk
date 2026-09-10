unset x
case yes in yes) echo yes;; ${x:=no}) echo no;; esac
printf "x=<%s>\n" "$x"
