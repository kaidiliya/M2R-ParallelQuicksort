OUTPUT_DIRECTORY=data/`hostname`_`date +%F`
mkdir -p $OUTPUT_DIRECTORY
OUTPUT_FILE=$OUTPUT_DIRECTORY/measurements_random_`date +%R`.txt

touch $OUTPUT_FILE
for i in 100 1000 10000 100000 1000000; do
for rep in `seq 1 5`; do
echo $i
done
done | shuf | while read i; do
echo "Size: $i" >> $OUTPUT_FILE;
./src/parallelQuicksort $i >> $OUTPUT_FILE;
done