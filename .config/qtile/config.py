# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
import subprocess
import os
from libqtile import hook

terminal = "kitty"

# AUTOSTART
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.run([home])


# KEY BINDINGS
mod = "mod4"  # "Windows" key

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Resize Windows
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    # Reset window size
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    # Terminal
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Browser
    Key([mod], "b", lazy.spawn("brave-browser"), desc="Launch browser"),
    # Application switcher
    Key([mod], "space", lazy.spawn("rofi -show"), desc="Launch Application Switcher"),
    # Application launcher
    Key([mod], "r", lazy.spawn("rofi -show drun"), desc="Launch Application Launcher"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    # Close current window
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    # Media Keys
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.spawn("amixer sset Master 2%-"),
        desc="Lower volume",
    ),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.spawn("amixer sset Master 2%+"),
        desc="Raise volume",
    ),
    Key(
        [],
        "XF86AudioMute",
        lazy.spawn("amixer -D pulse set Master toggle"),
        desc="Mute volume",
    ),
    Key(
        [],
        "XF86AudioPlay",
        lazy.spawn("playerctl play-pause"),
        desc="Play or pause music",
    ),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next"), desc="Next track"),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous"), desc="Previous track"),
    Key([], "XF86AudioStop", lazy.spawn("playerctl stop"), desc="Stop track"),
    # Screen brightness
    Key(
        [],
        "XF86MonBrightnessUp",
        lazy.spawn("brightnessctl set +5%"),
        desc="Raise screen brightness",
    ),
    Key(
        [],
        "XF86MonBrightnessDown",
        lazy.spawn("brightnessctl set 5%-"),
        desc="Lower screen brightness",
    ),
    # Take a screenshot
    Key([], "Print", lazy.spawn("flameshot gui"), desc="Take a screenshot"),
    # Lock computer
    Key([mod], "z", lazy.spawn("dm-tool lock"), desc="Lock the Computer"),
    # Eject flash drive
    Key(
        [mod],
        "e",
        lazy.spawn("fish /home/mjs/.config/qtile/eject.fish"),
        desc="Eject flash drive '/dev/sdb1'",
    ),
    Key(
        [mod, "control"],
        "p",
        lazy.spawn("fish /home/mjs/.config/qtile/reset_picom.fish"),
        desc="Restart the compositor",
    ),
    # Qtile controls
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

# WORKSPACES
groups = [Group(i) for i in ["", "", "", "", "", "", "", ""]]
group_hotkeys = "asdfuiop"

for g, k in zip(groups, group_hotkeys):
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                k,
                lazy.group[g.name].toscreen(),
                desc=f"Switch to group {g.name}",
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                k,
                lazy.window.togroup(g.name, switch_group=False),
                desc=f"Switch to & move focused window to group {g.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

# LAYOUTS

# First, some color
catppuccin = {
    "flamingo": "#F2CDCD",
    "mauve": "#DDB6F2",
    "pink": "#F5C2E7",
    "maroon": "#E8A2AF",
    "red": "#F28FAD",
    "peach": "#F8BD96",
    "yellow": "#FAE3B0",
    "green": "#ABE9B3",
    "teal": "#B5E8E0",
    "blue": "92CDFB",
    "sky": "#89DCEB",
    "white": "#D9E0EE",
    "gray0": "#6E6C7E",
    "black1": "#1A1826",
}

default_layout_config = {
    "border_focus": catppuccin["green"],
    "border_normal": catppuccin["red"],
    "border_width": 2,
    "margin": 3,
}

layouts = [
    layout.Columns(**default_layout_config),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(**default_layout_config),
    # layout.MonadWide(),
    layout.RatioTile(**default_layout_config),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

# PANEL


widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=16,
    padding=3,
    foreground=catppuccin["black1"],
)
extension_defaults = widget_defaults.copy()


def get_widgets(primary=False):
    widgets = [
        widget.Spacer(length=3, background="#00000000"),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["pink"],
            background="#00000000",
        ),
        widget.TextBox(
            text="異",
            mouse_callbacks={"Button1": lazy.spawn("rofi -show run")},
            background=catppuccin["pink"],
        ),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["pink"],
            background=catppuccin["red"],
        ),
        widget.CurrentLayoutIcon(
            padding=1,
            scale=0.8,
            background=catppuccin["red"],
            custom_icon_paths=["/home/mjs/.config/qtile/icons/"],
        ),
        widget.CurrentLayout(background=catppuccin["red"]),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["red"],
            background=catppuccin["peach"],
        ),
        widget.GroupBox(
            highlight_method="line",
            background=catppuccin["peach"],
            highlight_color=[catppuccin["peach"], catppuccin["peach"]],
            other_screen_border=catppuccin["gray0"],
            other_current_screen_border=catppuccin["white"],
            this_current_screen_border=catppuccin["white"],
            this_screen_border=catppuccin["gray0"],
            inactive=catppuccin["black1"],
        ),
        widget.TextBox(
            text="", padding=0, fontsize=30, foreground=catppuccin["peach"]
        ),
        widget.WindowName(fontsize=12, foreground=catppuccin["black1"]),
        widget.TextBox(text="", padding=0, fontsize=30, foreground=catppuccin["teal"]),
        widget.Volume(
            fmt="墳 {}",
            mute_command="amixer -D pulse set Master toggle",
            background=catppuccin["teal"],
        ),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["green"],
            background=catppuccin["teal"],
        ),
        widget.CPU(
            format=" {load_percent:04}%",
            mouse_callbacks={"Button1": lazy.spawn("kitty -e htop")},
            background=catppuccin["green"],
        ),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["yellow"],
            background=catppuccin["green"],
        ),
        widget.CapsNumLockIndicator(fmt=" {}", background=catppuccin["yellow"]),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["peach"],
            background=catppuccin["yellow"],
        ),
        widget.Clock(format=" %a %d %b %Y, %I:%M %p", background=catppuccin["peach"]),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["red"],
            background=catppuccin["peach"],
        ),
        widget.Battery(
            format="{char} {percent:2.0%}",
            charge_char="",
            discharge_char="",
            full_char="",
            unknown_char="",
            empty_char="",
            show_short_text=False,
            background=catppuccin["red"],
        ),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["pink"],
            background=catppuccin["red"],
        ),
        widget.TextBox(
            text="﫼",
            mouse_callbacks={
                "Button1": lazy.spawn("dm-tool lock"),
                "Button3": lazy.shutdown(),
            },
            background=catppuccin["pink"],
        ),
        widget.Spacer(length=10, background=catppuccin["pink"]),
        widget.TextBox(
            text="",
            mouse_callbacks={
                "Button1": lazy.spawn("systemctl suspend"),
                "Button2": lazy.spawn("systemctl restart"),
                "Button3": lazy.spawn("systemctl poweroff"),
            },
            background=catppuccin["pink"],
        ),
        widget.TextBox(
            text="",
            padding=0,
            fontsize=30,
            foreground=catppuccin["pink"],
            background="#00000000",
        ),
        widget.Spacer(length=3, background="#00000000"),
    ]
    if primary:
        widgets.insert(10, widget.Systray())
    return widgets


screens = [
    Screen(
        top=bar.Bar(
            get_widgets(primary=True),
            24,
            background="#00000000",
            margin=[2, 0, 0, 0],
        ),
    ),
    Screen(
        top=bar.Bar(
            # Use everything except the systray, which would crash
            get_widgets(primary=False),
            24,
            background="#00000000",
            margin=[2, 0, 0, 0],
        )
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
