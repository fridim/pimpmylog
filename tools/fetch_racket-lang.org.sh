#!/bin/sh
curl http://racket-lang.org/irc-logs/racket/ \
|egrep -o 'href="[0-9]+.txt"' \
|sed 's/href="//; s/"$//' \
|awk "\$1 > \"$(ls -1 20*.txt|tail -n 1)\"" \
|sed 's/^/http:\/\/racket-lang.org\/irc-logs\/racket\//; s/"$//' \
|xargs -n1 wget

for i in *.txt; do sed "s/^/$i/; s/.txt/ /;" $i; done \
|perl -pe 's/^(\d\d\d\d)(\d\d)(\d\d)/$1-$2-$3/' > racket.log
