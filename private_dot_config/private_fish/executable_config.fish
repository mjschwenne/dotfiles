status is-login; and begin

    # Login shell initialisation
    set -g -x PATH $PATH /home/mjs/.local/bin/
    set -g -x EDITOR /home/mjs/.local/bin/nvim
    set -g -x GTK_THEME catppuccin-mocha-pink-standard+default
    eval (ssh-agent -c)
end

status is-interactive; and begin

    # Abbreviations


    # Aliases
    alias cs400-auth 'gcloud auth login'
    alias cs400-ssh 'gcloud compute ssh --zone "us-central1-a" "cs400-vm" --project "cs-400-398116"'
    alias dup-files 'find . -type f -printf '\''%p -> %f\n'\'' | sort -k2 | uniq -f1 --all-repeated=separate'
    alias flatten 'find */ -type f -exec sh -c '\''file=${1#./}; mv "$file" "$(basename $file)"'\'' _ '\''{}'\'' \; ; find */ -depth -type d -exec rmdir '\''{}'\'' \;'
    alias icat swayimg
    alias ls eza
    alias m math
    alias mjs-bulk-rename 'find . -depth -exec fish -c '\''mjs-rename "{}"'\'' \;'
    alias nix-shell 'nix-shell --run fish'
    alias vi nvim

    # Interactive shell initialisation
    if test "$TERM" != dumb
        eval (starship init fish)

    end
end
