#!/usr/bin/env bash

# provides additional workspace management for sway.
# 
# Switch to the next or previous workspace:
#   workspace.sh navigate [previous|next]
#
# Moving the currently focused container to the next or previous workspace:
#   workspace.sh move [previous|next]
#
# Example Sway configuration:
# 
#   bindsym $mod+d exec .../workspace.sh switch next
#   bindsym $mod+Shift+d exec .../workspace.sh move next
#   bindsym $mod+u exec .../workspace.sh switch previous
#   bindsym $mod+Shift+u exec .../workspace.sh move previous

MAX_WORKSPACE=5

# Find the target workspace, looping if necessary
# Args:
#   $1 = Direction (next or previous) or workspace number
function find_target {
    local FOCUSED=$(swaymsg -t get_workspaces | jq --raw-output '. | map(select(.focused == true)) | .[0].num')
    local HIGHEST=$(swaymsg -t get_workspaces | jq --raw-output '. | .[].num' | sort -n -r | head -n1)
    local MAX=$(( HIGHEST > MAX_WORKSPACE ? HIGHEST : MAX_WORKSPACE ))

    local TARGET
    if [[ $1 == "next" ]]; then
        TARGET=$((FOCUSED + 1))
        if [[ $TARGET -gt $MAX ]]; then
            TARGET=1
        fi
    elif [[ $1 == "previous" ]]; then
        TARGET=$((FOCUSED - 1))
        if [[ $TARGET -lt 1 ]]; then
            TARGET=$MAX
        fi
    else
        TARGET=$1
    fi

    echo $TARGET
}

# Switch to another workspace
# Args:
#   $1 = Direction (next or previous) or workspace number
function switch {
    local TARGET=$(find_target $1)
    swaymsg "workspace $TARGET"
}

# Move the focused container to another workspace
# Args:
#   $1 = Direction (next or previous) or workspace number
function move {
    local TARGET=$(find_target $1)
    swaymsg  "move container to workspace $TARGET"
}

if [[ $1 == "switch" ]]; then
    switch $2
elif [[ $1 == "move" ]]; then
    move $2
else
    echo "unknown command"
    exit 1
fi
