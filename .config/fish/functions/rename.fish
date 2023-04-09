# function _rename -d 'nicer version of rename'
#     if test (count $argv) -lt 3
#         echo 'not enough arguments provided'
#         return 1
#     end

#     for file in $argv[2]
#         command mv $file (echo $file | sed -E $argv[1])
#     end
# end
