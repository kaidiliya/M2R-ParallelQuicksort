OUTPUT_DIRECTORY=data/`hostname`_`date +%F`
mkdir -p $OUTPUT_DIRECTORY
OUTPUT_FILE=$OUTPUT_DIRECTORY/measurements_all_`date +%R`.txt

touch $OUTPUT_FILE
for i in 100 300 1000 3000 10000 30000 100000 300000 1000000 3000000; do
for rep in `seq 1 30`; do
echo $i
done
done | shuf | while read i; do
echo "Size: $i" >> $OUTPUT_FILE;
./src/parallelQuicksort $i >> $OUTPUT_FILE;
done