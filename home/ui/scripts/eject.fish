#!/etc/profiles/per-user/mjs/bin/fish

if test $hostname = mjs-surface
	set eject_dirve '/dev/sda1'
else if test $hostname = sol
	set eject_dirve '/dev/sdb1'
end

if findmnt -S $eject_dirve > /dev/null
	notify-send "Ejecting $eject_dirve..."
	udisksctl unmount -b $eject_dirve
end

udisksctl power-off -b $eject_dirve
