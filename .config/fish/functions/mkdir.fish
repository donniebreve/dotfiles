function mkdir -d 'Create a directory and set CWD'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    command mkdir $argv
    if test $status = 0
        switch $argv[(count $argv)]
            case '-*'
            case '*'
                cd $argv[(count $argv)]
                return
        end
    end
end
