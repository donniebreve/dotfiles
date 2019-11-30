# Exports
export CLICOLOR=1

# Auto completion
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' max-errors 1 numeric
zstyle ':completion:*' prompt '%e'
zstyle :compinstall filename '/home/don/.zshrc'
autoload -Uz compinit
compinit

# History at current cursor
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search

# Git information
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%r%f'
zstyle ':vcs_info:*' enable git

# Standard configuration
bindkey -v
# del, home, end
bindkey "\e[3~" delete-char
bindkey "\e[H"  beginning-of-line
bindkey "\eOH"  beginning-of-line
bindkey "\e[1~" beginning-of-line
bindkey "\e[F"  end-of-line
bindkey "\eOF"  end-of-line
bindkey "\e[4~" end-of-line

# Aliases
alias ls='ls -laF --color=auto'

# Pacman shortcuts
#alias pacman-install='sudo pacman -S'
#alias pacman-search-local='pacman -Qs'
#alias pacman-search-online='pacman -Ss'
#alias pacman-remove='sudo pacman -Rns'
#alias pacman-update='sudo pacman -Syu'
#alias pacman-mirror-update='reflector --country US --protocol https -i .edu --sort rate | sudo tee /etc/pacman.d/mirrorlist'
pacman() {
    if [[ $1 == "install" ]]; then
        command sudo pacman -S $2
    elif [[ $1 == "installed" ]]; then
        command pacman -Qs $2
    elif [[ $1 == "uninstall" ]]; then
        command sudo pacman -Rns $2
    elif [[ $1 == "update" ]]; then
        command sudo pacman -Syu
    elif [[ $1 == "mirrors" ]]; then
        command sudo reflector --country US --protocol https -i .edu --sort rate | sudo tee /etc/pacman.d/mirrorlist
    fi
}

# Transmission shortcuts
trm() {
    command transmission-remote $1
}

# Prompt
#   Bash
#       Default='\033[0m'
#       Green='\033[32m';
#       Red='\033[31m';
#       Blue='\033[34m';
#       Yellow='\033[33m';
#       PS1="[${Blue}\h ${Blue}\t${Default}][${Yellow}\w${Default}]: "
#   Zsh
#       %B  - bold
#       %F  - color
#       %m  - machine name
#       %f  - stop color
#       %2~ - last two sections of pwd
#       %b  - stop bold
#       %#  - % regular, # sudo
#       RPROMPT - right side, git information
PROMPT='%B[%F{yellow}%m%f][%F{grey}%2~]%f%b %# '
RPROMPT=\$vcs_info_msg_0_
