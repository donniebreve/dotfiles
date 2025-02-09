#!/bin/bash

# Sway config
rsync --verbose --delete --recursive ~/.config/sway/ ./.config/sway/

# Neovim config
rsync --verbose --delete --recursive --exclude lazy-lock.json ~/.config/nvim/ ./.config/nvim/
