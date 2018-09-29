#!/bin/bash

file=screenshot_$(date -Iseconds).png

maim -s | tee ~/Pictures/screenshots/$file | xclip -selection clipboard -t image/png
notify-send "Screenshot taken" "$file"