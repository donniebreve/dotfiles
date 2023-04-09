function util -d 'various utility commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    switch $argv[1]
        case 'toggle'
            # toggle a service
            if test (systemctl --user status $argv[2] | rg 'Active: active')
                command systemctl --user stop $argv[2]
                return
            else if test (systemctl --user status $argv[2] | rg 'Active: inactive')
                command systemctl --user start $argv[2]
                return
            end
            if test (sudo systemctl status $argv[2] | rg 'Active: active')
                command sudo systemctl stop $argv[2]
                return
            else if test (sudo systemctl status $argv[2] | rg 'Active: inactive')
                command sudosystemctl start $argv[2]
                return
            end
            return
        case 'print'
            # toggle printing services
            if test (sudo systemctl status cups.service | rg 'Active: active')
                command sudo systemctl stop cups.socket
                command sudo systemctl stop cups.service
                command sudo systemctl stop avahi-daemon.socket
                command sudo systemctl stop avahi-daemon.service
            else if test (sudo systemctl status cups.service | rg 'Active: inactive')
                command sudo systemctl start cups.service
                command sudo systemctl start avahi-daemon.service
            end
            return
    end
end
