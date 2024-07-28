#!/usr/bin/fish 

# General icons
wget -qO- https://git.io/papirus-icon-theme-install | env DESTDIR="$HOME/.icons" sh

# Catppuccin folders 
git clone https://github.com/catppuccin/papirus-folders.git
cd papirus-folders

cp -rL src/* $HOME/.icons/Papirus/
curl -LO https://raw.githubusercontent.com/PapirusDevelopmentTeam/papirus-folders/master/papirus-folders
chmod +x ./papirus-folders
env PREFIX=$HOME/.icons ./papirus-folders -C cat-mocha-pink --theme Papirus-Dark

# Remove git repo
cd ..
rm -rf papirus-folders
