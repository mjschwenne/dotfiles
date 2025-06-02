{
  config,
  pkgs,
  ...
}: {
  # System level GUI requirements

  environment.pathsToLink = ["/libexec"];

  fonts.fontconfig.enable = true;
  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
  ];

  environment.systemPackages = with pkgs; [where-is-my-sddm-theme];

  services = {
    xserver = {
      enable = true;
      desktopManager = {
        xterm.enable = false;
      };
      displayManager = {
        # sddm = {
        #   enable = true;
        # };
        autoLogin = {
          enable = true;
          user = "mjs";
        };
        defaultSession = "none+i3";
      };
      windowManager.i3.enable = true;
      xkb = {
        layout = "us";
        variant = "";
      };
    };
    # greetd = {
    #   enable = true;
    #   settings = {
    #     default_session = {
    #       command = ''${pkgs.greetd.tuigreet}/bin/tuigreet --remember --time --cmd "startx"'';
    #       user = "mjs";
    #     };
    #   };
    # };
    autorandr = {
      enable = true;
      profiles.default.config.default.scale = {
        y = 1;
        x = 1;
      };
    };
  };

  programs.kdeconnect.enable = true;

  security.pam.services = {
    greetd = {
      gnupg.enable = true;
    };
    login = {
      gnupg.enable = true;
    };
  };
}
