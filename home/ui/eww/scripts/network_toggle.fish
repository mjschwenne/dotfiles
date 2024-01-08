#!/run/current-system/sw/bin/fish

if test (nmcli networking connectivity) = full
    nmcli networking off
else
    nmcli networking on
end
