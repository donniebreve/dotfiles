function cps -d 'nicer version of cp'
    command rsync -ah --relative --progress $argv
end
