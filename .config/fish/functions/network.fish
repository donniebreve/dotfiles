function network -d 'intuitive network commands'
  switch $argv[1]
    case 'wireguard'
      switch $argv[2]
        case 'add'
          command nmcli connection import type wireguard file $argv[3]
          return
      end
    case 'natpmp'
      switch $argv[2]
        case 'open'
          while true
            date
            set udp_output (natpmpc -a 1 0 udp 60 -g 10.2.0.1)
            string join \n $udp_output | grep -oP --color=never "Mapped public port \d+ protocol \w+"
            if test $status -ne 0
              echo "ERROR: Failed to request udp using natpmpc"
              break
            end
            set udp_port (echo $udp_output | grep -oP "Mapped public port \d+" | grep -oP "\d+")
            if test -z "$udp_port"
              echo "ERROR: Could not find mapped UDP port"
              break
            end
            set tcp_output (natpmpc -a 1 0 tcp 60 -g 10.2.0.1)
            string join \n $tcp_output | grep -oP --color=never "Mapped public port \d+ protocol \w+"
            if test $status -ne 0
              echo "ERROR: Failed to request tcp using natpmpc"
              break
            end
            set tcp_port (echo $tcp_output | grep -oP "Mapped public port \d+" | grep -oP "\d+")
            if test -z "$tcp_port"
              echo "ERROR: Could not find mapped TCP port"
              break
            end
            if test "$udp_port" != "$tcp_port"
              echo "ERROR: Mapped UDP port ($udp_port) does not match Mapped TCP port ($tcp_port)"
              break
            end
            set -g allowed_port $tcp_port
            sudo ufw status | grep -q "$allowed_port.*ALLOW"
            if test $status -ne 0
              sudo ufw allow $allowed_port
              echo "Added UFW rule for port: $tcp_port"
            end
            echo "Waiting 45s..."
            sleep 45
          end
          return
        case 'close'
          if test -n "$allowed_port"
            sudo ufw delete allow $allowed_port
            echo "Removed UFW rule for port: $allowed_port"
          end
          return
        case '*'
          echo "Invalid option"
          return
      end
    case '*'
      command nmcli $argv
      return
  end
end
