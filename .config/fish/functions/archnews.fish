# Prints news items from Arch Linux (https://archlinux.org/feeds/news/)
# Inspired by: https://github.com/depau/arch-news-printer/blob/master/archnews.py

function archnews_prettydate
    set date (date -d "$argv[1]" +%s)
    set now (date +%s)
    set delta (math "$now - $date")

    if test $delta -lt 3600
        echo (math "round($delta / 60)")"m ago"
    else if test $delta -lt 86400
        echo (math "round($delta / 3600)")"h ago"
    else if test $delta -lt 604800
        echo (math "round($delta / 86400)")"d ago"
    else if test $delta -lt 2592000
        echo (math "round($delta / 604800)")"w ago"
    else if test $delta -lt 31536000
        echo (math "round($delta / 2592000)")"mo ago"
    else
        echo (math "round($delta / 31536000)")"y ago"
    end
end

function archnews_hyperlink
    echo -e "\033]8;;$argv[1]\033\\$argv[2]\033]8;;\033\\"
end

function archnews_format_item
    set title (echo $argv[1] | xmllint --xpath 'string(//title)' -)
    set date (echo $argv[1] | xmllint --xpath 'string(//pubDate)' -)
    set link (echo $argv[1] | xmllint --xpath 'string(//link)' -)
    set description (echo $argv[1] | xmllint --xpath 'string(//description)' -)
    echo -e "\033[1m * \033[36m$(archnews_hyperlink $link $title) \033[0m($(archnews_prettydate $date))"
    echo (echo $description | sed 's/<[^>]*>//g')
    echo ""
end

function archnews_format_item_short
    set title (echo $argv[1] | xmllint --xpath 'string(//title)' -)
    set date (echo $argv[1] | xmllint --xpath 'string(//pubDate)' -)
    set link (echo $argv[1] | xmllint --xpath 'string(//link)' -)
    echo -e "\033[1m * \033[36m$(archnews_hyperlink $link $title) \033[0m($(archnews_prettydate $date))"
end

function archnews -d 'arch news commands'
    argparse -n archnews 'h/help' 'l/long=' 's/short=' -- $argv
    or return

    set long 0 
    set short 0

    if set -q _flag_help
      echo "Print Arch Linux's news RSS feed"
      echo "Usage: archnews [--long count] [--short count]"
      echo "Example: archnews: Pretty print all items"
      echo "Example: archnews --long 3: Fully print three items"
      echo "Example: archnews --long 3 --short 2: Fully print three items, then shortly print two items"
      echo "Example: archnews --long 0 --short 2: Shortly print two items"
    end

    if set -q _flag_long
        set long $_flag_long
    end

    if set -q _flag_short
        set short $_flag_short
    end

    if test $long -le 0 -a $short -le 0
      return
    end

    set response (curl -s "https://archlinux.org/feeds/news/")
    set items (echo $response | xmllint --xpath '//item' -)

    if test $long -gt 0
      for i in (seq 1 $long)
        set item $items[$i]
        archnews_format_item $item
      end
    end

    if test $short -gt 0
      set end (math "$long + $short")
      set start (math "$long + 1")
      for i in (seq $start $end)
        set item $items[$i]
        archnews_format_item_short $item
      end
    end
end
