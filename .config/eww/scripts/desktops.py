#!/usr/bin/env python

import subprocess as sp
from string import Template

eww_template = Template('(button :class "$css_class" :onclick "~/.config/bspwm/scripts/select-desktop.fish $id" :width 25 "$disp") ')

def process_report (report):
    """
    Generate eww from bspc report

    Parameters
    ----------
    report : str 
        One line output by `bspc subscribe rule`

    Returns
    -------
    str 
        eww yuck for the desktop status
    """
    desktop_eww = '(box :class "desktops" :orientation "h" :space-evenly false :spacing 5 :width 300 :valign "center" '

    desktop_status = {}
    focused_monitor = False
    for string in report[1:].split(":"):
        if string[0] == "M":
            focused_monitor = True
        elif string[0] == "m":
            focused_monitor = False
        elif string[0] in "oOfFuU":
            try: 
                desktop = int(string[1:])
            except ValueError:
                # If the desktop isn't a number, it must be the temp desktop 
                # which should not be reported on the eww widget.
                continue
            if string[0] in "ofu":
                desktop_status[desktop] = string[0]
            elif string[0] in "OFU":
                if focused_monitor:
                    desktop_status[desktop] = "F"
                else:
                    desktop_status[desktop] = "A"

    for k, v in sorted(desktop_status.items(), key=lambda i: i[0]):
        match v:
            # Desktop is occupied but not active or focused
            case 'o':
                desktop_eww += eww_template.substitute(css_class="desktop-occupied",
                                                       id=k, disp=k)
            case 'f':
                desktop_eww += eww_template.substitute(css_class="desktop-free", 
                                                       id=k, disp="")
            case 'u':
                desktop_eww += eww_template.substitute(css_class="desktop-urgent", 
                                                       id=k, disp=k)
            case 'F':
                desktop_eww += eww_template.substitute(css_class="desktop-focused",
                                                       id=k, disp=k)
            case 'A':
                desktop_eww += eww_template.substitute(css_class="desktop-active",
                                                       id=k, disp=k)

    print(desktop_eww + ")", flush=True)

bspc = sp.Popen(["bspc", "subscribe", "report"], stdout=sp.PIPE, 
                encoding="utf-8", errors="backslashreplace")

while True:
    process_report(bspc.stdout.readline())
    if bspc.poll() is not None:
        break

