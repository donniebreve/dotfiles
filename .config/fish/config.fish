set -gx EDITOR nvim

set __fish_git_prompt_show_informative_status
set __fish_git_prompt_showcolorhints
set __fish_git_prompt_showupstream "informative"

function fish_greeting -d "The greeting"
    if not set -q NO_GREETING
        figlet -f slant (hostnamectl hostname)
        fortune
        echo
    end
end

function fish_prompt_section_host_time
    printf '[%s%s %s%s]' (set_color blue) (uname -n | cut -d . -f 1) (date +%H:%M) (set_color normal)
end

function fish_prompt_section_cwd
    printf '[%s%s%s]' (set_color green) (prompt_pwd) (set_color normal)
end

function fish_prompt_section_git
    if test (fish_vcs_prompt)
        printf '%s%s%s' (set_color yellow) (fish_vcs_prompt | cut -b 2-) (set_color normal)
    end
end

function fish_prompt -d "The prompt"
    fish_prompt_section_host_time
    fish_prompt_section_cwd
    fish_prompt_section_git
    echo ': '
end

function edit -d "alias edit to nvim"
  command nvim $argv
end

. $HOME/.config/fish/plugins/marks.fish

function launch -d "launch a window manager"
    if test (count $argv) -lt 1
        echo "no argument provided"
        return 1
    end

    switch $argv[1]
        case 'sway'
            set -gx XDG_SESSION_DESKTOP sway
            set -gx XDG_CURRENT_DESKTOP sway
            set -gx QT_QPA_PLATFORM=wayland
            command sway
            return
        case 'gnome'
            set -lx XDG_SESSION_TYPE wayland
            command dbus-run-session gnome-session
            return
        case 'gnome-x'
            command startx 
            return
        case 'help'
            echo 'options:'
            echo 'update'
            echo 'scan'
    end
end
