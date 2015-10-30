exec > Makefile
echo get=./get.sh
get=./get.sh

if [ "${gt}" == "" ]
then
    gt=UnterInternet
fi
g= 
for f in *.sc
do
     s=$(echo $f | cut -d'.' -f 1)
     t=${s}internet.txt
     g="$g $t"
done
echo ${gt}: $g
echo "	awk '1' $g > $gt.txt"
for f in *.sc
do
     s=$(echo $f | cut -d'.' -f 1)
     t=${s}internet.txt
     echo $t: $f $get
     echo "	\${get}  $f $t "
done
