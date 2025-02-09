function pacman -d 'intuitive pacman commands'
    switch $argv[1]
        case 'search'
            command pacman -Ss $argv[2..-1]
            return
        case 'find'
            command pacman -Qs $argv[2..-1]
            return
        case 'install'
            command sudo pacman -S $argv[2..-1]
            return
        case 'remove'
            command sudo pacman -Rns $argv[2..-1] 
            return
        case 'status'
            command checkupdates
            return
        case 'update'
            archnews
            read -P "Press any key to continue..."
            command sudo pacman -Syu
            if test $status -ne 0
              return
            end
            return
        case 'mirrors'
            command sudo reflector --connection-timeout 1 --download-timeout 1 --age 24 --delay 12 --country US --include '\.edu' --latest 5 --protocol https --threads 32 --save /etc/pacman.d/mirrorlist
            cat /etc/pacman.d/mirrorlist
            return
        case 'clean'
            command sudo pacman -Scc --noconfirm
            return
        # case 'maintenance'
        #     command sudo paccache -r
        #     command sudo pacman -Rns (pacman -Qtdq) # remove orphans, but these may be AUR requirements
        #     return
        case '*'
            command pacman $argv
            return
    end
end
