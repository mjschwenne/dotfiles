{ pkgs, stasis-pkg }:
pkgs.writeShellApplication {
  name = "power-menu";
  runtimeInputs = [
    pkgs.niri
    pkgs.fuzzel
    pkgs.systemd
  ];
  text = ''
    choice=$(printf "%s\n" \
      "󰌾  Lock" \
      "󰤄  Suspend" \
      "󰍃  Logout" \
      "󰜉  Reboot" \
      "󰐥  Shutdown" \
      | fuzzel --dmenu --prompt "⏻  ")

    case "$choice" in
      *Lock)     ${stasis-pkg}/bin/stasis trigger suspend ;;
      *Suspend)  systemctl suspend ;;
      *Logout)   niri msg action quit ;;
      *Reboot)   systemctl reboot ;;
      *Shutdown) systemctl poweroff ;;
    esac
  '';
}
