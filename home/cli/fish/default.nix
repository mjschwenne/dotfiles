{osConfig, ...}: {
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      fish_config theme choose Nord
      zoxide init fish | source
    '';
    shellAliases = {
      vi = "nvim";
      dup-files = "find . -type f -printf '%p -> %f\\n' | sort -k2 | uniq -f1 --all-repeated=separate";
      flatten = ''
        find */ -type f -exec sh -c 'file=''${1#./}; mv "$file" "$(basename $file)"' _ '{}' \; ; find */ -depth -type d -exec rmdir '{}' \;'';
      mjs-bulk-rename = ''find . -depth -exec fish -c 'mjs-rename "{}"' \;'';
      icat = "kitty +kitten icat";
      m = "math";
      nix-shell = "nix-shell --run fish";
      ls = "eza";
      ssh = ''kitty +kitten ssh -i ${osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path}'';
      scp = ''scp -i ${osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path}'';
    };
    functions = {
      mjs-git = ''
        if test -n "$CONTAINER_ID"
          flatpak-spawn --host --env=GIT_SSH_COMMAND=/run/current-system/sw/bin/ssh /etc/profiles/per-user/mjs/bin/git $argv
        else
          git $argv
        end
      '';
      mjs-rename = ''
        for src in $argv
            set -l filename (string split -r -m 1 -f 1 '.' (basename $src))
            set -l extension (string split -r -m 1 -f 2 '.' (basename $src))
            # Remove '(', ')', '_', and "'" characters from the file name
            # Replace ' <letter>' with '-<letter>'
            # Remove any remaining spaces
            # Replace all '.' and ',' characters with '-'
            # Replace camelCase with kebab case
            # Remove extra consecutive '-' characters
            # Remove leading '-'
            # Lowercase any remaining capital letters
            # Replace '&' with 'and'
            # Separate numbers from letters
            # Unseprarte if it's marking the size of a ttrpg asset
            set -l dest (echo $filename | sed -E \
                -e "s/[()_']//g" \
                -e "s/ ([a-zA-Z])/\-\L\1/g" \
                -e "s/ //g" \
                -e "s/[\.,]/\-/g" \
                -e "s/([a-z])([A-Z]+)/\1\-\L\2/g" \
                -e "s/\-\-+/\-/g" \
                -e "s/^\-//g" \
                -e "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/" \
                -e "s/&/and/g" \
                -e "s/([a-z])([0-9]+)/\1-\2/" \
                -e "s/([0-9]+x)-([0-9]+)/\1\2/g"
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
      fish_greeting = "fastfetch -c examples/8.jsonc";
    };
  };
}
