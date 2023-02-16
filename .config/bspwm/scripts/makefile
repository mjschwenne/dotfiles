# This makefile is used to tangle org files which make different config 
# depending on which system they are being built on

DOTFILES=~/.config/eww/eww.scss\
		 ~/.config/eww/eww.yuck\
		 ~/.config/bspwm/poly.ini

all: $(DOTFILES)

% : %.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "$<")'
