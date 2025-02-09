function stream -d 'helpful stream commands'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end
    switch $argv[1]
        case 'video'
            command yt-dlp --format 'bestvideo[height=720][fps=60]+bestaudio/bestvideo[height=720][fps=30]+bestaudio' --output - $argv[2] | tee video.mp4 | vlc -
            return
        case 'audio'
            command yt-dlp --extract-audio --output - $argv[2] | tee audio.mp4 | vlc -
            return
    end
 end
