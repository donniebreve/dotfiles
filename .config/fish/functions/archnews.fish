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

function archnews_should_refresh
    set file $argv[1]
    if test -e $file
        set current_time (date +%s)
        set file_mod_time (stat -c %Y $file)
        if test (math "$current_time - $file_mod_time") -gt 3600
            true
        else
            false
        end
    else
        true
    end
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
    argparse -n archnews h/help 'l/long=' 's/short=' -- $argv
    or return

    set long -1
    set short -1

    if set -q _flag_help
        echo "Print Arch Linux's news RSS feed"
        echo "Usage: archnews [--long count] [--short count]"
        echo "Example: archnews: Pretty print all items"
        echo "Example: archnews --long 3: Fully print three items"
        echo "Example: archnews --long 3 --short 2: Fully print three items, then shortly print two items"
        echo "Example: archnews --long 0 --short 2: Shortly print two items"
    end

    set tmp $TMPDIR
    if test -z "$TMPDIR"
        set tmp /tmp
    end
    set archnews_file "$tmp/archnews.rss"
    if archnews_should_refresh $archnews_file
        set response (curl -s "https://archlinux.org/feeds/news/")
        echo $response > $archnews_file
    else
        set response (cat $archnews_file)
    end

    set items (echo $response | xmllint --xpath '//item' -)

    if set -q _flag_long
        set long $_flag_long
    else
        set long 3 
    end

    for i in (seq 1 $long)
        set item $items[$i]
        archnews_format_item $item
    end

    if set -q _flag_short
        set short $_flag_short
        set end (math "$long + $short")
        set start (math "$long + 1")
        for i in (seq $start $end)
            set item $items[$i]
            archnews_format_item_short $item
        end
    end
end
