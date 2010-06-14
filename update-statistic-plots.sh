#! /bin/sh

# png output is hard-coded in "plots" subdirectory of current dir

BASE_DIR="/home/saros-statistics"
SOURCE_DATA_DIR="$BASE_DIR/data/saros-statistic"
AGGREGATED_DIR="$BASE_DIR/statistics-aggregated"
SCRIPT_DIR="$BASE_DIR/statProcStandAlone"

# this is hard-coded in StatAggregator
AGGREGATED_FILENAME="data.txt"

set -e

cd $BASE_DIR

mkdir -p $AGGREGATED_DIR
rm -f $AGGREGATED_DIR/$AGGREGATED_FILENAME

java -jar $SCRIPT_DIR/StatAggregator.jar $SOURCE_DATA_DIR $AGGREGATED_DIR

# plots dir is hard-coded in StatAggregator
rm -f $BASE_DIR/plots/*

Rscript --vanilla $SCRIPT_DIR/Analysis.R $AGGREGATED_DIR/$AGGREGATED_FILENAME
