// This is for the cool memory indicator on the sidebar
// For the right pill of the bar, see system.js
const { Gdk, Gtk } = imports.gi;
import { SCREEN_HEIGHT, SCREEN_WIDTH } from '../../imports.js';
import App from 'resource:///com/github/Aylur/ags/app.js';
import Widget from 'resource:///com/github/Aylur/ags/widget.js';
import * as Utils from 'resource:///com/github/Aylur/ags/utils.js';

const { exec, execAsync } = Utils;

const SessionButton = (name, icon, command, props = {}) => {
    const buttonDescription = Widget.Revealer({
        vpack: 'end',
        transitionDuration: 200,
        transition: 'slide_down',
        revealChild: false,
        child: Widget.Label({
            className: 'txt-smaller session-button-desc',
            label: name,
        }),
    });
    return Widget.Button({
        onClicked: command,
        className: 'session-button',
        child: Widget.Overlay({
            className: 'session-button-box',
            child: Widget.Label({
                vexpand: true,
                className: 'icon-material',
                label: icon,
            }),
            overlays: [
                buttonDescription,
            ]
        }),
        onHover: (button) => {
            const display = Gdk.Display.get_default();
            const cursor = Gdk.Cursor.new_from_name(display, 'pointer');
            button.get_window().set_cursor(cursor);
            buttonDescription.revealChild = true;
        },
        onHoverLost: (button) => {
            const display = Gdk.Display.get_default();
            const cursor = Gdk.Cursor.new_from_name(display, 'default');
            button.get_window().set_cursor(cursor);
            buttonDescription.revealChild = false;
        },
        setup: (self) => self
            .on('focus-in-event', (self) => {
                buttonDescription.revealChild = true;
                self.toggleClassName('session-button-focused', true);
            })
            .on('focus-out-event', (self) => {
                buttonDescription.revealChild = false;
                self.toggleClassName('session-button-focused', false);
            })
        ,
        ...props,
    });
}

export default () => {
    // lock, logout, sleep
    // const lockButton = SessionButton('Lock', 'lock', () => { App.closeWindow('session'); execAsync('gtklock') });
    const lockButton = SessionButton('Lock', 'lock', () => { App.closeWindow('session'); execAsync('swaylock') });
    const logoutButton = SessionButton('Logout', 'logout', () => { App.closeWindow('session'); execAsync(['bash', '-c', 'pkill Hyprland || pkill sway']) });
    const sleepButton = SessionButton('Sleep', 'sleep', () => { App.closeWindow('session'); execAsync('systemctl suspend') });
    // hibernate, shutdown, reboot
    const hibernateButton = SessionButton('Hibernate', 'downloading', () => { App.closeWindow('session'); execAsync('systemctl hibernate') });
    const shutdownButton = SessionButton('Shutdown', 'power_settings_new', () => { App.closeWindow('session'); execAsync('systemctl poweroff') });
    const rebootButton = SessionButton('Reboot', 'restart_alt', () => { App.closeWindow('session'); execAsync('systemctl reboot') });
    const cancelButton = SessionButton('Cancel', 'close', () => App.closeWindow('session'), { className: 'session-button-cancel' });
    return Widget.Box({
        className: 'session-bg',
        css: `
        min-width: ${SCREEN_WIDTH * 1.5}px; 
        min-height: ${SCREEN_HEIGHT * 1.5}px;
        `, // idk why but height = screen height doesn't fill
        vertical: true,
        children: [
            Widget.EventBox({
                onPrimaryClick: () => App.closeWindow('session'),
                onSecondaryClick: () => App.closeWindow('session'),
                onMiddleClick: () => App.closeWindow('session'),
            }),
            Widget.Box({
                hpack: 'center',
                vexpand: true,
                vertical: true,
                children: [
                    Widget.Box({
                        vpack: 'center',
                        vertical: true,
                        className: 'spacing-v-15',
                        children: [
                            Widget.Box({
                                vertical: true,
                                css: 'margin-bottom: 0.682rem;',
                                children: [
                                    Widget.Label({
                                        className: 'txt-title txt',
                                        label: 'Session',
                                    }),
                                    Widget.Label({
                                        justify: Gtk.Justification.CENTER,
                                        className: 'txt-small txt',
                                        label: 'Use arrow keys to navigate.\nEnter to select, Esc to cancel.'
                                    }),
                                ]
                            }),
                            Widget.Box({
                                hpack: 'center',
                                className: 'spacing-h-15',
                                children: [ // lock, logout, sleep
                                    lockButton,
                                    logoutButton,
                                    sleepButton,
                                ]
                            }),
                            Widget.Box({
                                hpack: 'center',
                                className: 'spacing-h-15',
                                children: [ // hibernate, shutdown, reboot
                                    hibernateButton,
                                    shutdownButton,
                                    rebootButton,
                                ]
                            }),
                            Widget.Box({
                                hpack: 'center',
                                className: 'spacing-h-15',
                                children: [ // hibernate, shutdown, reboot
                                    cancelButton,
                                ]
                            }),
                        ]
                    })
                ]
            })
        ],
        setup: (self) => self
            .hook(App, (_b, name, visible) => {
                if (visible) lockButton.grab_focus(); // Lock is the default option
            })
        ,
    });
}
