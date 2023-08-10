#!/usr/bin/bash

OUT_DIR="inputs"
[ -d $OUT_DIR ] || mkdir $OUT_DIR

DAY=$1

YEAR=$2
[ -z "$YEAR" ] && YEAR="2022"

if [ -z "$ADVENT_SESSION" ]; then
	echo "Make sure to set ADVENT_SESSION"
	exit 1
fi

curl https://adventofcode.com/"$YEAR"/day/"$DAY"/input \
	--silent \
	--cookie session=$ADVENT_SESSION \
	--output "$OUT_DIR/day$DAY.txt"
