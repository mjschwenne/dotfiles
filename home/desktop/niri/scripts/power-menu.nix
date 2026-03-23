{ pkgs, stasis-pkg }:
pkgs.writeShellApplication {
  name = "power-menu";
  runtimeInputs = [
    pkgs.niri
    pkgs.fuzzel
    pkgs.systemd
  ];
  text = ''
    placeholder="$(date '+ %a %m-%d  󰥔 %R')"
    choice=$(printf "%s\n" \
      "󰌾  Lock" \
      "󰤄  Suspend" \
      "󰍃  Logout" \
      "󰜉  Reboot" \
      "󰐥  Shutdown" \
      | fuzzel --dmenu --prompt "⏻  " --placeholder "         $placeholder")

    case "$choice" in
      *Lock)     ${stasis-pkg}/bin/stasis trigger suspend ;;
      *Suspend)  systemctl suspend ;;
      *Logout)   niri msg action quit ;;
      *Reboot)   systemctl reboot ;;
      *Shutdown) systemctl poweroff ;;
    esac
  '';
}
