#!/usr/bin/fish

flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

set flatpaks "com.brave.Browser" "com.github.tchx84.Flatseal" "com.protonvpn.www" "org.mozilla.Thunderbird" "ch.protonmail.protonmail-bridge" "io.github.Qalculate" "com.github.xournalpp.xournalpp" "org.zotero.Zotero" "dev.vencord.Vesktop" "com.valvesoftware.Steam" "net.kuribo64.melonDS" "io.github.Cockatrice.cockatrice"
for f in $flatpaks
    flatpak install -y flathub $f
end
