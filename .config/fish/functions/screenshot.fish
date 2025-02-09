function screenshot -d 'a flameshot wrapper'
    set -x XDG_SESSION_TYPE wayland
    set -x XDG_SESSION_DESKTOP sway
    set -x XDG_CURRENT_DESKTOP sway
    set -x MOZ_ENABLE_WAYLAND 1
    set -x QT_QPA_PLATFORM wayland
    set -x SDL_VIDEODRIVER wayland
    set -x _JAVA_AWT_WM_NONREPARENTING 1

    if test (count $argv) -lt 1
        command flameshot gui
        return
    end

    switch $argv[1]
        case 'gui'
            command flameshot gui
            return
        case '*'
            command flameshot $argv
            return
    end
end
