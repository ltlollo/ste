funcs() { cscope -kL0 ".*" $@ | awk '{ print $2 }' | grep -v "<global>"| sort -u; }
calls() { while read i; do cscope -kL3 $i | awk -vc="$i" '{ printf "%s->%s;\n",$2,c }'; done; }
graph() { echo "digraph iftree {"; funcs $@ | calls | sort -u; echo "}"; } > "dot$1"
graph "$1" && dot -Grankdir=LR -Gordering=out -Tpng "dot$1" -o "g$1.png" && feh "g$1.png"
