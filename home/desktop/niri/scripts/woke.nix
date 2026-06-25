{ pkgs, stasis-pkg }:
pkgs.writeShellApplication {
  name = "woke";
  text = ''
    ${stasis-pkg}/bin/stasis pause
    "$@"
    ${stasis-pkg}/bin/stasis resume
  '';
}
