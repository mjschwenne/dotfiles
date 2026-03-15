{ pkgs }:
pkgs.writeShellApplication {
  name = "window-switcher";
  runtimeInputs = [
    pkgs.niri
    pkgs.fuzzel
    pkgs.jq
  ];
  text = ''
    windows=$(niri msg -j windows)

    count=$(echo "$windows" | jq 'length')
    if [[ "$count" -eq 0 ]]; then
      exit 0
    fi

    choice=$(echo "$windows" \
      | jq -rj '.[] | (.app_id | split(".") | last) as $name | "\($name): \(.title)\u0000icon\u001f\(.app_id)\n"' \
      | fuzzel --dmenu --index --prompt "󰕰  ")

    if [[ -z "$choice" ]]; then
      exit 0
    fi

    index=''${choice%%:*}
    id=$(echo "$windows" | jq -r ".[$index].id")
    niri msg action focus-window --id "$id"
  '';
}
