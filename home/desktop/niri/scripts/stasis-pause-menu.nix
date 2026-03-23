{ pkgs, stasis-pkg }:
pkgs.writeShellApplication {
  name = "stasis-pause-menu";
  runtimeInputs = [
    pkgs.fuzzel
  ];
  text = ''
    choice=$(printf "%s\n" \
      "󰒲  15 minutes" \
      "󰒲  30 minutes" \
      "󰒲  1 hour" \
      "󰒲  2 hours" \
      "󰒲  Until time..." \
      "󰒲  Indefinitely" \
      | fuzzel --dmenu --prompt "󰒲  " --placeholder "Pause idle timers...")

    case "$choice" in
      *"15 minutes")  ${stasis-pkg}/bin/stasis pause for 15m ;;
      *"30 minutes")  ${stasis-pkg}/bin/stasis pause for 30m ;;
      *"1 hour")      ${stasis-pkg}/bin/stasis pause for 1h ;;
      *"2 hours")     ${stasis-pkg}/bin/stasis pause for 2h ;;
      *"Until time...")
        time=$(printf "" | fuzzel --dmenu --prompt "Until: " --placeholder "1:30pm or 13:30")
        if [ -n "$time" ]; then
          ${stasis-pkg}/bin/stasis pause until "$time"
        fi
        ;;
      *"Indefinitely") ${stasis-pkg}/bin/stasis pause ;;
    esac
  '';
}
