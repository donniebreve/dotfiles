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
            # https://wiki.archlinux.org/title/ClamAV#Using_clamdscan
            command sudo systemctl start clamav-daemon.service
            command clamdscan --multiscan --fdpass $argv[2]
            return
        case 'log'
            command journalctl --unit clamav-daemon --since -30m --reverse
            return
        case 'slow-scan'
            command clamscan -r $argv[2]
            return
        case 'help'
            echo 'options:'
            echo 'update'
            echo 'scan'
            return
    end
end
