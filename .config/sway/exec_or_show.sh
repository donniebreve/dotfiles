#!/usr/bin/bash

if [ "$#" -ne 2 ]; then
  echo "Error: This script requires exactly two arguments."
  exit 1
fi

if ! pgrep -x $2 > /dev/null; then
    swaymsg workspace $1
    swaymsg exec $2
else
    # Wow! jq is crazy
    swaymsg "[pid=$(swaymsg -t get_tree | jq --raw-output "recurse | select(.type? == \"con\" and ((if .app_id then (.app_id | test(\"$2\";\"i\")) else false end) or (if .name then (.name | test(\"$2\";\"i\")) else false end))) | .pid")]" focus
fi
