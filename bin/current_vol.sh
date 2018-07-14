#!/bin/bash

mute_template=$1

sink=0
vol=$(pactl list sinks | grep '^[[:space:]]Volume:' | \
    head -n $(( $sink + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')
mute=$(pactl list sinks | grep '^[[:space:]]Mute:' | \
    head -n $(( $sink + 1 )) | tail -n 1 | cut -d ' ' -f2)

if [ "$mute" == "no" ]; then
    echo "vol: $vol%"
else
    echo "vol: $vol% $mute_template"
fi