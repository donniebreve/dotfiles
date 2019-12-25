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
alias bonsai='./bonsai.sh -n -L 20 -g 35,20 > /tmp/bonsai.txt | neofetch --ascii /tmp/bonsai.txt --ascii_colors 11 3 10 2 0'

# The Fuck
fuck () {
    TF_PYTHONIOENCODING=$PYTHONIOENCODING;
    export TF_SHELL=zsh;
    export TF_ALIAS=fuck;
    TF_HISTORY="$(fc -ln -10)";
    export TF_HISTORY;
    export PYTHONIOENCODING=utf-8;
    TF_CMD=$(
        thefuck THEFUCK_ARGUMENT_PLACEHOLDER $@
    ) && eval $TF_CMD;
    unset TF_HISTORY;
    export PYTHONIOENCODING=$TF_PYTHONIOENCODING;
    test -n "$TF_CMD" && print -s $TF_CMD
}

# Pacman shortcuts
pacman() {
    if [[ $1 == "install" ]]; then
        command sudo pacman -S ${@:2}
    elif [[ $1 == "search" ]]; then
        command pacman -Qs ${@:2}
    elif [[ $1 == "uninstall" ]]; then
        command sudo pacman -Rs ${@:2}
    elif [[ $1 == "update" ]]; then
        command sudo pacman -Syu
    elif [[ $1 == "mirrors" ]]; then
        command sudo reflector --country US --protocol https -i .edu --sort rate | sudo tee /etc/pacman.
    elif [[ $1 == "cleanup" ]]; then
        command sudo pacman -Sc 
    else
        echo "invalid command"
    fi  
}


# Transmission shortcuts
trm() {
    command transmission-remote ${@:1}
}

# Virtualbox
vbox() {
    if [[ $1 == "start" ]]; then
        command vboxmanage startvm $2 --type:headless
    elif [[ $1 == "shutdown" ]]; then
        command vboxmanage controlvm $2 acpipowerbutton
    elif [[ $1 == "list" ]]; then
        if [[ $2 == "verbose" ]]; then
            command vboxmanage list -l runningvms
        else
            command vboxmanage list runningvms
        fi
    elif [[ $1 == "poweroff" ]]; then
        command vboxmanage controlvm $2 poweroff
    elif [[ $1 == "info" ]]; then
        command vboxmanage showvminfo $2
    elif [[ $1 == "insert-ga" ]]; then
        command vboxmanage storageattach $2 --storagectl IDE --port 0 --device 0 --type dvddrive --medium "/usr/lib/virtualbox/additions/VBoxGuestAdditions.iso"
    elif [[ $1 == "remove-ga" ]]; then
        command vboxmanage storageattach $2 --storagectl IDE --port 0 --device 0 --type dvddrive --medium "none"
    fi
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
