function ls -d 'Use eza for ls'
    set -gx EXA_COLORS "xx=''"
    command eza --long --header --no-permissions --no-user --time=modified --time-style=iso --group-directories-first --icons $argv[1..]
end
