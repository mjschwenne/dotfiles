if status is-interactive
    # Commands to run in interactive sessions can go here
end

# alias for dotfile tracking
alias config="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"

# start the ssh-agent
if test -z (pgrep ssh-agent)
	eval (ssh-agent -c)
	set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
  	set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

# set the EDITOR variable to be nvim
export EDITOR="/usr/bin/nvim"
export PATH="$PATH:/home/mjs/.spicetify"
