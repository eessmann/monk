echo "before"
( time echo "timed" | tr a-z A-Z ) 2>/dev/null
echo "after $?"
