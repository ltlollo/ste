funcs() { cscope -kL0 ".*" $1 | awk '{ print $2 }' | grep -v "<global>"| sort -u; }
calls() { while read i; do cscope -L3 $i| awk -v c="$i" '{ printf "%s->%s\n",$2,c }'; done; }
graph() { echo "digraph iftree {"; funcs "$1" | calls | sort -u; echo "}"; } > "dot$1"
graph "$1" && dot -Grankdir=LR -Gordering=out -Tpng "dot$1" -o "g$1.png" && feh "g$1.png"
