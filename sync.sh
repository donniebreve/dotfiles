#!/bin/bash

# Sway config
rsync --verbose --delete --recursive ~/.config/sway/ ./.config/sway/

# Fish config
rsync --verbose --delete --recursive --exclude fish_variables ~/.config/fish/ ./.config/fish/

# Neovim config
rsync --verbose --delete --recursive --exclude lazy-lock.json ~/.config/nvim/ ./.config/nvim/
