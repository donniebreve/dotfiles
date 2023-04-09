function ls -d 'Use exa for ls'
    command exa --long --header --no-permissions --no-user --time=modified --time-style=iso --group-directories-first --icons $argv[1..]
end
