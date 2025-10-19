#!/bin/bash

# Get the screen width
screen_width=$(swaymsg -t get_outputs | jq -r '.[0].rect.width')

# Calculate the widths for the left and right windows
left_width=$((screen_width * 70 / 100))
right_width=$((screen_width * 30 / 100))

# Get the current workspace's top level node(s)
top_nodes=($(swaymsg -t get_tree | jq -r '.. | select(.type? and .type == "workspace") as $workspace | .. | select(.focused? and .focused == true) | $workspace | .nodes[].id'))
echo "${top_nodes[@]}"
echo ${#top_nodes[@]}

if [ ${#top_nodes[@]} -ne 2 ]; then
  echo "Not two windows"
  exit
fi

# Resize the left window
swaymsg "[con_id=${top_nodes[0]}] resize set $left_width $((screen_width * 100 / 100))"

# Resize the right window
swaymsg "[con_id=${top_nodes[1]}] resize set $right_width $((screen_width * 100 / 100))"
