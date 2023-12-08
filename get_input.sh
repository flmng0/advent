#!/usr/bin/bash

DAY=$1
YEAR=$2
[ -z "$YEAR" ] && YEAR="2023"

BASE_DIR="inputs"

[ -d "$BASE_DIR/$YEAR" ] || mkdir -p "$BASE_DIR/$YEAR"

if [ -z "$ADVENT_SESSION" ]; then
	echo "Make sure to set ADVENT_SESSION"
	exit 1
fi

curl https://adventofcode.com/"$YEAR"/day/"$DAY"/input \
	--silent \
	--cookie session=$ADVENT_SESSION \
	--output "$BASE_DIR/$YEAR/day$DAY.txt"
