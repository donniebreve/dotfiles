function la -d 'Use eza for la'
    set -gx EXA_COLORS "xx=''"
    command eza --long --header -all --time=modified --time-style=iso --group-directories-first --icons $argv
end
