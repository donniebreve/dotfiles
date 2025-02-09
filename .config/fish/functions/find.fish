function find -d 'easier find commands'
    switch $argv[1]
        case 'file'
            command fd --type f --ignore-case --hidden --threads 32 "$argv[2]*" .
            return
        case 'dir'
            command fd --type d --ignore-case --hidden --threads 32 "$argv[2]*" .
            return
        case '*'
            command fd $argv
            return
    end
end
