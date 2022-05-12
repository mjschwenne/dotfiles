#! /usr/bin/fish

if findmnt -S /dev/sdb1 > /dev/null
	udisksctl unmount -b /dev/sdb1
end

udisksctl power-off -b /dev/sdb1
