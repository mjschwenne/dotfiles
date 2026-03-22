{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "mjs-fuzzel";
  runtimeInputs = [
    pkgs.fuzzel
  ];
  text = ''
    placeholder="$(date '+ %a %m-%d  󰥔 %R')"
    fuzzel --placeholder "         $placeholder"
  '';
}
