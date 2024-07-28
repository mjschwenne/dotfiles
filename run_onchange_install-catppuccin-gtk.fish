#!/usr/bin/fish 

mkdir -p $HOME/.local/share/themes
cd $HOME/.local/share/themes

set URL "https://github.com/catppuccin/gtk/releases/download"
set RELEASE "v1.0.3"
set FLAVOR mocha
set ACCENT pink

wget "$URL/$RELEASE/catppuccin-$FLAVOR-$ACCENT-standard+default.zip"
unzip catppuccin-$FLAVOR-$ACCENT-standard+default.zip
