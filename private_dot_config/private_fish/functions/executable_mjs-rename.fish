function mjs-rename
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
            set dest (string join '' (dirname $src) '/' $dest)
            if test -n $extension
                set dest (string join '.' $dest $extension)
            end

            if test $src != $dest
                mv -n $src $dest 2>&1 >/dev/null
            end
        end
    end
end
