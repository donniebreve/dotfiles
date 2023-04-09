function keeb -d "just some kayboards"
    if test (count $argv) -lt 1
        echo "no argument provided"
        return 1
    end

    switch $argv[1]
        case 'func'
            command echo 2 | sudo tee /sys/module/hid_apple/parameters/fnmode
            return
        case 'help'
            echo 'options:'
            echo 'function'
    end
end
