function python -d "helpful python commands"
    if test (count $argv) -lt 1
        echo "no argument provided"
        return 1
    end
    switch $argv[1]
        case "venv"
          switch $argv[2]
            case "create"
              command python -m venv --system-site-packages venv
              source ./venv/bin/activate.fish
              command python -m pip install --upgrade pip
              return
            case "activate"
              source ./venv/bin/activate.fish
              return
            case "deactivate"
              deactivate
              return
          end
        case "*"
            command python $argv
            return
    end
 end
