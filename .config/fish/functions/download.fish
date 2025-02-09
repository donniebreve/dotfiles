function download -d 'helpful download commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end
    switch $argv[1]
        case 'video'
            command yt-dlp --format 'bestvideo[height<=720][fps=60]+bestaudio' --output '%(title)s.%(ext)s' $argv[2]
            return
        case 'audio'
            command yt-dlp --extract-audio --output '%(title)s.%(ext)s' $argv[2]
            return
    end
end