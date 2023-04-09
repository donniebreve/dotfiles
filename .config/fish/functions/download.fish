function download -d 'helpful download commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end

    switch $argv[1]
        case 'audio'
            command yt-dlp -x -o '%(title)s.%(ext)s' $argv[2]
            return
    end
 end

