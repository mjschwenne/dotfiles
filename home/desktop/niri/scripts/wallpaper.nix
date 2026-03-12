{ pkgs, awww }:
pkgs.writeShellApplication {
  name = "wallpaper";
  runtimeInputs = [
    awww
    pkgs.procps
  ];
  text = ''
    transitions=(simple fade left right top bottom wipe wave grow center any outer)
    wallpaper_dir="$HOME/.dotfiles/home/desktop/wallpapers"

    random_choice() {
      local arr=("$@")
      echo "''${arr[RANDOM % ''${#arr[@]}]}"
    }

    set_wallpaper() {
      awww img -t "$(random_choice "''${transitions[@]}")" "$(find "$wallpaper_dir" -maxdepth 1 -type f | shuf -n1)"
    }

    if ! pgrep -x awww-daemon >/dev/null; then
      awww-daemon &
      set_wallpaper
    fi

    case "''${1:-}" in
      change)
        set_wallpaper
        ;;
      interval)
        while true; do
          set_wallpaper
          sleep "''${2:?Usage: wallpaper interval <seconds>}"
        done
        ;;
    esac
  '';
}
