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
            command pacman -Qu
            return
        case 'update'
            command sudo pacman -Syu 
            return
        case 'mirrors'
            command sudo reflector --country US --protocol https -i .edu --sort rate | sudo tee /etc/pacman.d/mirrorlist 
            return
        case 'clean'
            command sudo paccache -r
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
