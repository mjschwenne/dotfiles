if status is-interactive
    # Commands to run in interactive sessions can go here
	starship init fish | source

	# alias for dotfile tracking
	alias config="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
	alias vi="nvim"
	alias dup-files="find . -type f -printf '%p -> %f\n' | sort -k2 | uniq -f1 --all-repeated=separate"
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
export PATH="$PATH:/home/mjs/.spicetify:/home/mjs/.local/bin:/home/mjs/.cargo/bin"
alias m="math"

