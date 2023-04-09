function clam -d 'simplified clamAV commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    switch $argv[1]
        case 'update'
            command sudo freshclam
            return
        case 'scan'
            command clamdscan -m --fdpass $argv[2]
            return
        case 'help'
            echo 'options:'
            echo 'update'
            echo 'scan'
            return
    end
end
