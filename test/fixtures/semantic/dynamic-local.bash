x=global
g() { x=changed; }
f() { local x=local; g; echo "$x"; }
f
echo "$x"
