function screensaver -d 'runs terminal screen savers'
    if test (count $argv) -lt 1
        echo 'no argument provided'
        return 1
    end
    switch $argv[1]
        case 'bonsai'
          command bonsai.sh --life 32 --live --infinite
          return
        case 'lava'
          command lavat -c yellow -k red -F @@::::
          return
        case 'matrix'
          command cmatrix -b
          return
    end
end
