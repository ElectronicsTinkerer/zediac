#!/bin/bash

# RUN THIS SCRIPT TO UPDATE THE KICAD FILE TO YOUR FILE NAME!
# USAGE:
# $ ./RUNME.sh NEW_BOARD_NAME
#

TEMPLATE_NAME="zediac-template"
NEW_NAME="$1"

sed -i "s/$TEMPLATE_NAME/$NEW_NAME/g" "$TEMPLATE_NAME".*


echo "$TEMPLATE_NAME -> $NEW_NAME"
j=*
n=0
for i in $j; do
	if [[ "$i" == *"$TEMPLATE_NAME"* ]]; then
		k=$(echo "$i" | sed "s/$TEMPLATE_NAME/$NEW_NAME/g")
		echo "$i -> $k"
		mv "$i" "$k"
		n=$((n+1))
	fi
done

echo "Processed $n files"

