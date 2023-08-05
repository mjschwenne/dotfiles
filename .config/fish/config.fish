if status is-interactive
    # Commands to run in interactive sessions can go here
	starship init fish | source

	# alias for dotfile tracking
	alias config="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
	alias vi="nvim"
	alias mjs-emacs="emacs --with-profile default"
	alias doom-emacs="emacs --with-profile doom"
	alias nano-emacs="emacs --with-profile nano"
	alias spacemacs="emacs --with-profile spacemacs"
	alias dup-files="find . -type f -printf '%p -> %f\n' | sort -k2 | uniq -f1 --all-repeated=separate"
	alias flatten='find */ -type f -exec sh -c \'file=${1#./}; mv "$file" "$(basename $file)"\' _ \'{}\' \; ; find */ -depth -type d -exec rmdir \'{}\' \;'
	alias mjs_bulk_rename='find . -depth -exec fish -c \'mjs_rename "{}"\' \;'
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

function mjs_rename
	for src in $argv 
		set -l filename (string split -r -m 1 -f 1 '.' (basename $src))
		set -l extension (string split -r -m 1 -f 2 '.' (basename $src))
		# Remove '(', ')', '_', ' ' and "'" characters from the file name 
		# Replace all '.' characters with '-'
		# Replace camelCase with kebab case
		# Remove extra consecutive '-' characters
		# Remove leading '-' 
		# Remove leading '-' from subsequent parts of the file path
		# Lowercase any remaining capital letters 
		# Replace '&' with 'and'
		set -l dest (echo $filename | sed -E \
			-e "s/[()_']//g" \
			-e "s/ [a-z]/\-\l&/g" \
		    -e "s/ //g" \
			-e "s/[\.,]/\-/g" \
			-e "s/([a-z])([A-Z]+)/\1\-\L\2/g" \
			-e "s/\-\-+/\-/g" \
			-e "s/^\-//g" \
			-e "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/" \
			-e "s/&/and/g"
		)

		if test -n $dest
			set dest (string join '' (dirname $src) '/' $dest)
			if test -n $extension
				set dest (string join '.' $dest $extension)
			end
			
			if test $src != $dest 
				mv -n $src $dest 2>&1 > /dev/null
			end 
		end
	end
end
