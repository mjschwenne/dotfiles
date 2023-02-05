#!/bin/env python 

import subprocess as sp 
import re 

volume_status_regexp = re.compile(r'Right:.*\[([0-9]+)%\] \[(on|off)\]')

def process_volume():
    status = volume_status_regexp.search(sp.run(["amixer", "sget", "Master"], stdout=sp.PIPE, 
                       encoding='utf-8', errors="backslashreplace").stdout)
    mute = status.group(2) == "off"
    vol = status.group(1)
    icon = "ﱝ" if mute else "󰕾"
    tooltip = f"VOL: MUTED ({vol}%)" if mute else f"VOL: {vol}%"

    print(f'(circular-metric :icon "{icon}" :value {vol} :tooltip "{tooltip}" :css-class "{"volume-muted" if mute else "volume-unmuted"}" :cmd "amixer -D pulse set Master 1+ toggle")', flush=True)

# The basic idea here is to subsribe to the events of the Master audio channel, 
# which will generate a new line whenever the volume is changed but will NOT 
# report the new volume, then manually check the volume and report it to eww.

amixer = sp.Popen(["alsactl", "monitor"], stdout=sp.PIPE, stderr=sp.STDOUT,
                  encoding="utf-8", errors="backslashreplace")

process_volume()
while True:
    if amixer.stdout.readline() is not None and amixer.poll() is None:
        process_volume()
    else:
        break

