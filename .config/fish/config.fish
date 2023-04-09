set -x EDITOR nvim

set fish_color_command blue
set fish_color_param gray
set fish_color_error gray

set __fish_git_prompt_show_informative_status
set __fish_git_prompt_showcolorhints
set __fish_git_prompt_showupstream "informative"

set __fish_git_prompt_color_branch magenta
set __fish_git_prompt_color_cleanstate green
set __fish_git_prompt_color_stagedstate red
set __fish_git_prompt_color_invalidstate red
set __fish_git_prompt_color_untrackedfiles cyan
set __fish_git_prompt_color_dirtystate blue

function fish_greeting -d "The greeting"
    figlet -f slant (hostnamectl hostname)
    fortune
    echo
end

function fish_prompt -d "Write out the prompt"
    printf '[%s%s %s%s][%s%s%s]: ' (set_color blue) (uname -n | cut -d . -f 1) (date +%H:%M) (set_color normal)\
        (set_color green) (prompt_pwd) (set_color normal)
end

