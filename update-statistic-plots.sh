#! /bin/sh

SOURCE_DATA_DIR="/home/saros-statistics/data/saros-statistic"
AGGREGATED_DIR="/home/saros-statistics/statistics-aggregated"
OUTPUT_DIR="/home/saros-statistics/statistics-plots"

# this is hard-coded in StatAggregator
AGGREGATED_FILENAME="data.txt"


mkdir -p $AGGREGATED_DIR
rm -f $AGGREGATED_DIR/$AGGREGATED_FILENAME

java -jar StatAggregator.jar $SOURCE_DATA_DIR $AGGREGATED_DIR
rm -f $OUTPUT_DIR/*

Rscript --vanilla Analysis.R $AGGREGATED_DIR/$AGGREGATED_FILENAME
