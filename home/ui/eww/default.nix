{osConfig, ...}: {
  home.file = {
    ".config/eww/assets" = {
      source = ./assets;
      recursive = true;
    };

    ".config/eww/scripts" = {
      source = ./scripts;
      recursive = true;
    };

    ".config/eww/eww.scss".source = ./eww.scss;
    ".config/eww/luna.yuck".source = ./luna.yuck;
    ".config/eww/mars.yuck".source = ./mars.yuck;
    ".config/eww/terra.yuck".source = ./terra.yuck;
    ".config/eww/variables.yuck".source = ./variables.yuck;
    ".config/eww/widgets.yuck".source = ./widgets.yuck;
    ".config/eww/windows.yuck".source = ./windows.yuck;

    ".config/eww/eww.yuck".text = ''
      (include "./variables.yuck")
      (include "./${osConfig.networking.hostName}.yuck")
      (include "./windows.yuck")
      (include "./widgets.yuck")
    '';
  };
}
