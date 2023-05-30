function find -d 'easier find commands'
    switch $argv[1]
        case 'file'
            command find . -type f -iname "$argv[2]*"
            return
        case 'dir'
            command find . -type d -iname "$argv[2]*"
            return
        case '*'
            command find $argv
            return
    end
end
