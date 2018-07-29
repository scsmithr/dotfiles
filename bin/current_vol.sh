#!/bin/bash

mute_template=$1

getdefaultsinkname() {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

sink=0
vol=$(pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'$(getdefaultsinkname)'>"}
            /^\s+volume: / && indefault {print $5; exit}')
mute=$(pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'$(getdefaultsinkname)'>"}
            /^\s+muted: / && indefault {print $2; exit}')

if [ "$mute" == "no" ]; then
    echo "vol: $vol"
else
    echo "vol: $vol $mute_template"
fi