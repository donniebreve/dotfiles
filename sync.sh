#!/bin/bash

# Sway config
rsync --verbose --delete --recursive ~/.config/sway/ ./.config/sway/
