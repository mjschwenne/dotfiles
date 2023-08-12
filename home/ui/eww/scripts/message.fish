#!/etc/profiles/per-user/mjs/bin/fish

echo $argv > /tmp/serverin.fifo
echo (read < /tmp/serverout.fifo)
