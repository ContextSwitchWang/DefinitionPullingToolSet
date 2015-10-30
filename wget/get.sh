csvwriter=../ParserQ
replace=../replace
parsewiki=../ParseWiki
url='https://en.wiktionary.org/w/index.php?search='
pattern='index.php?search='
varDir=~/.cache/wiktiongrab/
tmp=/tmp/sdf55
tmp2=/tmp/pni91
tmp4=/tmp/parsed11
tmp3=$varDir/log
src=Unter2.sc
dst=Unter.txt
if [ "$1" != "" ]
then 
    src=$1
fi
if [ "$2" != "" ]
then
    dst=$2
fi
nr=$(wc -l $src |awk '{print $1}')
mkdir -p $varDir

echo "" > $dst
for ((c=0; c<= nr; c++ ))
do
            
   word=$( awk "NR == $c && \$0 !~ /#/  " $src )
   if [ "$word" != ""    ]
   then 
       word=$($replace "$word")
       echo "parsing $word"
       wget "$url$word" -kN -P $varDir -o $tmp3
       filename=$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$word")
       $parsewiki parse "$varDir$pattern$filename"  > $tmp
       echo $word > $tmp2
       $csvwriter $tmp2 $tmp >> $dst
       echo >> $dst
   fi 
done
