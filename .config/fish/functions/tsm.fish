function tsm -d 'simplified transmission-remote commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    switch $argv[1]
        case 'up'
            command transmission-daemon
        case 'down'
            command transmission-remote --exit
        case 'add'
            command transmission-remote --add $argv[2..-1]
        case 'remove'
            set --local start_index $argv[2]
            set --local end_index $argv[2]
            if string match --quiet --regex '(?<sind>\d+)..(<eind>\d+)' $argv[2] > /dev/null
                set start_index = $sind
                set end_index = $eind
            end
            set --local index $start_index
            while test $index -le $end_index
                command transmission-remote -t$index --remove
                set index (math $index + 1)
            end
        case 'start'
            set --local start_index $argv[2]
            set --local end_index $argv[2]
            if string match --quiet --regex '(?<sind>\d+)..(<eind>\d+)' $argv[2] > /dev/null
                set start_index = $sind
                set end_index = $eind
            end
            set --local index $start_index
            while test $index -le $end_index
                command transmission-remote -t$index --start
                set index (math $index + 1)
            end
        case 'stop'
            set --local start_index $argv[2]
            set --local end_index $argv[2]
            if string match --quiet --regex '(?<sind>\d+)..(<eind>\d+)' $argv[2] > /dev/null
                set start_index = $sind
                set end_index = $eind
            end
            set --local index $start_index
            while test $index -le $end_index
                command transmission-remote -t$index --stop
                set index (math $index + 1)
            end
        case 'ls'
            command transmission-remote --list
        case 'watch'
            while true
                tput clear
                tput cup 0 0
                transmission-remote --list
                sleep 3
            end
        case 'config'
            command $EDITOR ~/.config/transmission-daemon/settings.json
        case 'help'
            echo 'options:'
            echo 'up'
            echo 'down'
            echo 'add'
            echo 'remove'
            echo 'start'
            echo 'stop'
            echo 'ls'
            echo 'watch'
            echo 'config'
        case '*'
            command transmission-remote $argv
    end
end
