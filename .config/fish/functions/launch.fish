function launch -d 'launch a window manager'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    switch $argv[1]
        case 'sway'
            command sway
            return
        case 'gnome'
            set -lx XDG_SESSION_TYPE wayland
            command dbus-run-session gnome-session
            return
        case 'gnome-x'
            command startx 
            return
    end
end

