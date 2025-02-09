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
            else if test (systemctl --user status $argv[2] | rg 'Active: inactive|Active: failed')
                command systemctl --user start $argv[2]
            else if test (sudo systemctl status $argv[2] | rg 'Active: active')
                command sudo systemctl stop $argv[2]
            else if test (sudo systemctl status $argv[2] | rg 'Active: inactive|Active: failed')
                command sudo systemctl start $argv[2]
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
        case 'qmkb'
            # convert and build
            cd ~/Projects/qmk_firmware/keyboards/boardsource/lulu/keymaps/donniebreve
            command bash json2c.sh
            cd ~/Projects/qmk_firmware
            command make -j30 boardsource/lulu/rp2040:donniebreve
            return
        case 'qmkf'
            # flash
            cd ~/Projects/qmk_firmware
            command make boardsource/lulu/rp2040:donniebreve:flash
            return
        case 'set-norm-perms'
            command fd --type d --hidden --exec chmod 0755 {} \; .
            command fd --type f --hidden --exec chmod 0644 {} \; .
            return
        case 'set-open-perms'
            command fd --type d --hidden --exec chmod 0777 {} \; .
            command fd --type f --hidden --exec chmod 0666 {} \; .
            return
    end
end
