{ config
, pkgs
, ...
}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # link all files in `./scripts` to `~/.config/i3/scripts`
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # link recursively
  #   executable = true;  # make all files executable
  # };

  # encode the file content in nix configuration file directly
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "Matt Schwennesen";
    userEmail = "mjschwenne@gmail.com";
  };

  imports = [ ./editors/neovim.nix ];

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    # here is some command line tools I use frequently
    # feel free to add your own or remove some of them
    tmux

    neofetch
    pfetch

    # archives
    zip
    unzip

    # utils
    ripgrep # recursively searches directories for a regex pattern
    exa # A modern replacement for ‘ls’
    fzf # A command-line fuzzy finder
    ranger # command-line file manager

    # networking tools
    dnsutils # `dig` + `nslookup`
    ldns # replacement of `dig`, it provide the command `drill`
    nmap # A utility for network discovery and security auditing

    # misc
    cowsay
    file
    which
    tree
    gawk
    zstd
    gnupg

    # nix related
    #
    # it provides the command `nom` works just like `nix
    # with more details log output
    nix-output-monitor

    htop
    btop # replacement of htop/nmon

    # system call monitoring
    strace # system call monitoring
    ltrace # library call monitoring
  ];

  programs.fish = {
    enable = true;
    shellAliases = {
      vi = "nvim";
      dup-files = "find . -type f -printf '%p -> %f\\n' | sort -k2 | uniq -f1 --all-repeated=separate";
      flatten = ''
        find */ -type f -exec sh -c 'file=''${1#./}; mv "$file" "$(basename $file)"' _ '{}' \; ; find */ -depth -type d -exec rmdir '{}' \;'';
      mjs_bulk_rename = ''find . -depth -exec fish -c 'mjs_rename "{}"' \;'';
      icat = "kitty +kitten icat";
      ssh = "kitty +kitten ssh";
      m = "math";
      nix-shell = "nix-shell --run fish";
    };
    functions = {
      mjs_rename = ''
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
                set dest (string join ''' (dirname $src) '/' $dest)
                if test -n $extension
                    set dest (string join '.' $dest $extension)
                end

                if test $src != $dest
                    mv -n $src $dest 2>&1 > /dev/null
                end
            end
        end
      '';
      fish_greeting = "pfetch";
    };
  };

  programs.bash = {
    enable = true;
    shellAliases = { vi = "nvim"; };
  };

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
