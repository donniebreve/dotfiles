#!/bin/bash

# Sway config
rsync --verbose --delete --recursive ./.config/sway/ ~/.config/sway/

# Alacritty config
rsync --verbose --delete --recursive ./.config/alacritty/ ~/.config/alacritty/

# WezTerm config
rsync --verbose --delete --recursive ./.config/wezterm/ ~/.config/wezterm/

# Fish config
rsync --verbose --delete --recursive --exclude fish_variables ./.config/fish/ ~/.config/fish/

# Tmux config
rsync --verbose ./.tmux.conf ~/.tmux.conf

# Neovim config
rsync --verbose --delete --recursive --exclude lazy-lock.json ./.config/nvim/ ~/.config/nvim/
