function vbox -d 'simplified virtualbox commands'
    switch $argv[1]
        case 'start'
            command vboxmanage startvm $argv[2] --type:headless
            return
        case 'shutdown'
            command vboxmanage controlvm $argv[2] acpipowerbutton
            return
        case 'list'
            switch $argv[2]
                case 'verbose'
                    command vboxmanage list -l runningvms
                    return
                case '*'
                    command vboxmanage list runningvms
                    return
            end
            return
        case 'poweroff'
            command vboxmanage controlvm $argv[2] poweroff
            return
        case 'info'
            command vboxmanage showvminfo $argv[2]
            return
        case 'insert-gcd'
            command vboxmanage storageattach $argv[2] --storagectl IDE --port 0 --device 0 --type dvddrive --medium "/usr/lib/virtualbox/additions/VBoxGuestAdditions.iso"
            return
        case 'eject-gcd'
            command vboxmanage storageattach $argv[2] --storagectl IDE --port 0 --device 0 --type dvddrive --medium "none"
            return
        case 'help'
            echo 'options:'
            echo 'list'
            echo 'start'
            echo 'shutdown'
            echo 'poweroff'
            echo 'info'
            echo 'insert-gcd'
            echo 'eject-gcd' 
        case '*'
            command vboxmanage $argv
            return
    end
end
