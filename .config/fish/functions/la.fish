function la -d 'Use exa for la'
    command exa --long --header -all --time=modified --time-style=iso --group-directories-first --icons $argv
end
