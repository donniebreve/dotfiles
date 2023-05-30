function protonup -d 'launch protonup-qt in a virtual env'
    # The python packaging system is terrible
    # Any application using python that isn't shipped by official Arch Linux repos is almost always broken
    cd ~/Packages/ProtonUp-Qt/
    source ./venv/default/bin/activate.fish
    python -m pupgui2
    deactivate
    cd ~
end
