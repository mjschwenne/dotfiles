export const keybindList = [[
    {
        "icon": "pin_drop",
        "name": "Workspaces: navigation",
        "binds": [
            { "keys": ["", "+", "#"], "action": "Go to workspace #" },
            { "keys": ["", "+", "(Scroll ↑↓)"], "action": "Go to workspace -1/+1" },
            { "keys": ["Ctrl", "", "+", "←"], "action": "Go to workspace on the left" },
            { "keys": ["Ctrl", "", "+", "→"], "action": "Go to workspace on the right" },
            { "keys": ["", "+", "PageUp"], "action": "Go to workspace on the left" },
            { "keys": ["", "+", "PageDown"], "action": "Go to workspace on the right" }
        ],
        "appeartick": 1
    },
    {
        "icon": "overview_key",
        "name": "Workspaces: management",
        "binds": [
            { "keys": ["", "Shift", "+", "#"], "action": "Move and follow window to workspace #" },
            { "keys": ["", "Alt", "+", "#"], "action": "Move window to workspace #" },
            { "keys": ["", "Alt", "+", "PageUp"], "action": "Move window to workspace on the left" },
            { "keys": ["", "Alt", "+", "PageDown"], "action": "Move window to workspace on the right" }
        ],
        "appeartick": 1
    },
    {
        "icon": "move_group",
        "name": "Windows",
        "binds": [
            { "keys": ["", "+", "hjkl"], "action": "Focus window in direction" },
            { "keys": ["", "+", "←↓↑→"], "action": "Focus window in direction" },
            { "keys": ["", "Shift", "+", "hjkl"], "action": "Swap window in direction" },
            { "keys": ["", "Shift", "+", "←↓↑→"], "action": "Swap window in direction" },
            { "keys": ["", "+", ";"], "action": "Split ratio -" },
            { "keys": ["", "+", "'"], "action": "Split ratio +" },
            { "keys": ["", "+", "Lmb"], "action": "Move window" },
            { "keys": ["", "+", "Rmb"], "action": "Resize window" },
            { "keys": ["", "+", "i"], "action": "Float" },
            { "keys": ["", "+", "u"], "action": "Fullscreen" },
            { "keys": ["", "Shift", "+", "u"], "action": "Fullscreen (Keep bar)" },
            { "keys": ["", "Shift", "+", "o"], "action": "Fake fullscreen" },
            { "keys": ["", "+", "p"], "action": "Pin" },
        ],
        "appeartick": 1
    }
],
[
    {
        "icon": "widgets",
        "name": "Widgets (AGS)",
        "binds": [
            { "keys": ["", "+", "r", "OR", "", "+", "Tab"], "action": "Toggle overview/launcher" },
            { "keys": ["Ctrl", "", "+", "a"], "action": "Restart AGS" },
            { "keys": ["Ctrl", "", "+", "r"], "action": "Restart Hyprland and AGS" },
            { "keys": ["", "+", "/"], "action": "Toggle this cheatsheet" },
            { "keys": ["", "+", "n"], "action": "Toggle system sidebar" },
            { "keys": ["", "+", "m"], "action": "Toggle music popup" },
			{ "keys": ["", "+", ","], "action": "Toggle colorscheme popup" },
            // { "keys": ["", "+", "K"], "action": "Toggle virtual keyboard" },
            { "keys": ["Ctrl", "Alt", "+", "Del"], "action": "Power/Session menu" },

            { "keys": ["Esc"], "action": "Exit a window" },
            { "keys": ["rightCtrl"], "action": "Dismiss/close sidebar" },

            { "keys": ["Ctrl", "", "+", "w"], "action": "Change wallpaper+colorscheme" },
        ],
        "appeartick": 2
    },
    {
        "icon": "construction",
        "name": "Utilities",
        "binds": [
            { "keys": ["PrtSc"], "action": "Screenshot  >>  editor" },
            { "keys": ["ALT", "+", "PrtSc"], "action": "Screenshot  >>  clipboard" },
            { "keys": ["Shift", "+", "PrtSc"], "action": "Full screenshot  >>  clipboard" },
            { "keys": ["", "Shift", "+", "s"], "action": "Screen snip  >>  clipboard" },
            { "keys": ["", "Shift", "Ctrl", "+", "t"], "action": "Image to text  >>  clipboard" },
            { "keys": ["", "Alt", "+", "c"], "action": "Color picker" },
            { "keys": ["", "+", "r"], "action": "Record region" },
            { "keys": ["", "Shift", "+", "r"], "action": "Record fullscreen" },
            { "keys": ["", "Shift", "Alt", "+", "r"], "action": "Record region with sound" },
        ],
        "appeartick": 2
    },
    // {
    //     "icon": "edit",
    //     "name": "Edit mode",
    //     "binds": [
    //         { "keys": ["Esc"], "action": "Exit Edit mode" },
    //         { "keys": ["#"], "action": "Go to to workspace #" },
    //         { "keys": ["Alt", "+", "#"], "action": "Dump windows to workspace #" },
    //         { "keys": ["Shift", "+", "#"], "action": "Swap windows with workspace #" },
    //         { "keys": ["Lmb"], "action": "Move window" },
    //         { "keys": ["Mmb"], "action": "Move window" },
    //         { "keys": ["Rmb"], "action": "Resize window" }
    //     ],
    //     "appeartick": 2
    // }
],
[
    {
        "icon": "apps",
        "name": "Apps",
        "binds": [
            { "keys": ["", "+", "Return"], "action": "Launch terminal: foot" },
            { "keys": ["", "+", "b"], "action": "Launch browser: Firefox" },
            { "keys": ["", "+", "e"], "action": "Launch editor: emacs" },
            { "keys": ["", "+", "n"], "action": "Launch editor: neovim" },
            { "keys": ["", "Control", "+", "s"], "action": "Launch settings: GNOME Control center" },
            { "keys": ["", "Control", "+", "v"], "action": "Launch audio: pavucontrol" }
        ],
        "appeartick": 3
    },
    {
        "icon": "keyboard",
        "name": "Typing",
        "binds": [
            { "keys": ["", "+", "v"], "action": "Clipboard history  >>  clipboard" },
            { "keys": ["", "+", "."], "action": "Emoji picker  >>  clipboard" },
        ],
        "appeartick": 3
    },
    {
        "icon": "terminal",
        "name": "Launcher actions",
        "binds": [
            { "keys": [">raw"], "action": "Toggle mouse acceleration" },
            { "keys": [">img"], "action": "Select wallpaper and generate colorscheme" },
            { "keys": [">light"], "action": "Switch to light theme" },
            { "keys": [">dark"], "action": "Switch to dark theme" },
            { "keys": [">badapple"], "action": "Apply black n' white colorscheme" },
            { "keys": [">color"], "action": "Pick acccent color" },
            { "keys": [">todo"], "action": "Type something after that to add a To-do item" },
        ],
        "appeartick": 3
    }
]];
