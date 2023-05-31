if status is-interactive
    # Commands to run in interactive sessions can go here
	starship init fish | source

	# alias for dotfile tracking
	alias config="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
	alias vi="nvim"
	alias mjs-emacs="emacs --with-profile mjs"
	alias doom-emacs="emacs --with-profile doom"
	alias nano-emacs="emacs --with-profile nano"
	alias spacemacs="emacs --with-profile spacemacs"
	alias dup-files="find . -type f -printf '%p -> %f\n' | sort -k2 | uniq -f1 --all-repeated=separate"
	alias flatten='find */ -type f -exec sh -c \'file=${1#./}; mv "$file" "$(basename $file)"\' _ \'{}\' \; ; find */ -depth -type d -exec rmdir \'{}\' \;'
	function rename2kb
		for src in $argv 
			# Remove '(', ')', '_' and ' ' characters from the file name 
			# Replace all '.' characters with '-'
			# Restore the last '-' to a '.'
			# Replace camelCase with kebab case
			# Remove extra consecutive '-' characters
			# Remove leading '-' 
			# Remove leading '-' from subsequent parts of the file path
			# Lowercase any remaining capital letters 
			# Replace '&' with 'and'
			set -l dest (echo (basename $src) | sed -E \
				-e "s/[()_ ]//g" \
				-e "s/\./\-/g" \
				-e "s/\-([^\-]*)\$/\.\1/" \
				-e "s/[A-Z]/\-\l&/g" \
				-e "s/\-\-+/\-/g" \
				-e "s/^\-//g" \
				-e "s/\/\-/\//g" \
				-e "y/A-Z/a-z/" \
				-e "s/&/and/g"
			)
			mv -n $src (string join (dirname "/" $src) $dest) > /dev/null
		end
	end
	export DOOMDIR="/home/mjs/.config/emacs-configs/doom-config/"
end


# alias for kitty kittens
alias icat="kitty +kitten icat"
alias kssh="kitty +kitten ssh"

# start the ssh-agent
if test -z (pgrep ssh-agent)
	eval (ssh-agent -c)
	set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
  	set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

# set the EDITOR variable to be nvim
export EDITOR="/usr/bin/nvim"
export PATH="$PATH:/home/mjs/.spicetify:/home/mjs/.local/bin:/home/mjs/.cargo/bin:/home/mjs/.local/share/gem/ruby/3.0.0/bin:"
alias m="math"

