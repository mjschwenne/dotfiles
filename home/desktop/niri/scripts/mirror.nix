{ pkgs }:
pkgs.writeShellApplication {
  name = "mirror";
  runtimeInputs = [
    pkgs.niri
    pkgs.wl-mirror
  ];
  text = ''
    get_display_names() {
      local re='Output.*\(([^)]+)\)'
      while IFS= read -r line; do
        if [[ "$line" =~ $re ]]; then
          echo "''${BASH_REMATCH[1]}"
        fi
      done < <(niri msg outputs)
    }

    get_external_display_name() {
      while IFS= read -r name; do
        if [[ "$name" != "eDP-1" ]]; then
          echo "$name"
          return
        fi
      done < <(get_display_names)
    }

    wl-mirror "eDP-1" 2>/dev/null &
    sleep 0.1

    wl_id=""
    while IFS= read -r line; do
      if [[ "$line" =~ Window\ ID\ ([0-9]+) ]]; then
        wl_id="''${BASH_REMATCH[1]}"
      fi
      if [[ "$line" == *'at.yrlf.wl_mirror'* ]] && [[ -n "$wl_id" ]]; then
        break
      fi
    done < <(niri msg windows)

    external=$(get_external_display_name)
    niri msg action move-window-to-monitor --id "$wl_id" "$external"
    niri msg action fullscreen-window --id "$wl_id"
    niri msg action focus-monitor eDP-1
  '';
}
