{
  pkgs,
  showBattery ? false,
  ...
}:
pkgs.writeShellApplication {
  name = "mjs-fuzzel";
  runtimeInputs = [
    pkgs.fuzzel
  ]
  ++ pkgs.lib.optionals showBattery [
    pkgs.upower
    pkgs.jc
    pkgs.jq
  ];
  text = ''
    placeholder="$(date '+ %a %m-%d  󰥔 %R')"
    leading="         "
    ${pkgs.lib.optionalString showBattery /* bash */ ''
      battery="$(upower -b | jc --upower | jq -r '[.[] | select(.detail.type == "battery") | .detail.percentage | floor] | first // empty')"
      if [ -n "$battery" ]; then
        suffix="  󰁹 $battery%"
        placeholder="$placeholder$suffix"
        leading="''${leading:$(( ''${#suffix} / 2 ))}"
      fi
    ''}
    fuzzel --placeholder "$leading$placeholder"
  '';
}
