function system -d 'system commands'
    switch $argv[1]
        case 'update'
            archnews
            read -P "Press any key to continue..."
            echo ""
            echo "-- pacman --"
            echo ""
            command sudo pacman -Syu
            if test $status -ne 0
              return
            end
            echo ""
            echo "-- paru --"
            echo ""
            command paru -Syu -a --sudoloop
            echo ""
            echo "-- flatpak --"
            echo ""
            command flatpak upgrade
            if test $status -ne 0
              return
            end
            echo ""
            echo "-- clamav --"
            echo ""
            command sudo freshclam
            if test $status -ne 0
              return
            end
            echo ""
            echo "-- cache --"
            command sudo pacman -Scc --noconfirm
            return
        case '*'
            return
    end
end
