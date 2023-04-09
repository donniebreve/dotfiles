#!/bin/bash
# aur installation helper script

echo "aur install helper script"

if [ -z "$1" ]
  then
    echo "no argument supplied"
    exit 1
fi

# build search url
url="https://aur.archlinux.org/rpc/?v=5&type=search&arg=${1}"
echo "searching $url"
echo ""

# get results
results=( $(curl -s $url | grep -Po '(?<="Name":")[^"]+(?=")') )

# display results to user
for i in "${!results[@]}"; do
    echo "$i:${results[$i]}"
done
read -p "selection: " selection
echo ""

# clone the repo
repo="https://aur.archlinux.org/${results[$selection]}.git"
destination="Packages/${results[$selection]}/"
echo "removing $destination"
rm -Rf $destination
echo "cloning $repo to $destination"
git clone $repo $destination
echo ""

# view the PKGBUILD
cd "$destination"
less PKGBUILD

# skip gpg signature
read -p "skip gpg? (y/n): " gpg

# build the package
read -p "build? (y/n): " build
if [ $build = "y" ]
then
    if [ $gpg = "y" ]
    then
        makepkg -si --skippgpcheck
    else
        makepkg -si
    fi
fi
