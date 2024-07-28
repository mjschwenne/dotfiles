#!/usr/bin/fish 

distrobox enter mjs-desktop -T -a -d -- sleep 1
distrobox enter mjs-desktop -T -a -d -- autotiling
distrobox enter mjs-desktop -T -a -d -- wpaperd
distrobox enter mjs-desktop -a "--env SSH_AUTH_SOCK=$SSH_AUTH_SOCK" -- keepassxc
