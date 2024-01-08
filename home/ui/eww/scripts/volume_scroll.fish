#!/run/current-system/sw/bin/fish

if test $argv[1] = up
    wpctl set-volume @DEFAULT_SINK@ 0.05+
else if test $argv[1] = down
    wpctl set-volume @DEFAULT_SINK@ 0.05-
end
