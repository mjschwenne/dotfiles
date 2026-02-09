{
  osConfig,
  pkgs,
  stylix,
  ...
}:
{
  imports = [
    stylix.homeModules.stylix

    ./cli
    ./editors
  ];

  home = {
    username = "mjs";
    homeDirectory = "/home/mjs";
  };

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/everforest.yaml";
    polarity = "dark";
    fonts = {
      serif = {
        package = pkgs.inriafonts;
        name = "Inria Serif";
      };
      sansSerif = {
        package = pkgs.inriafonts;
        name = "Inria Sans";
      };
      monospace = {
        package = pkgs.nerd-fonts.jetbrains-mono;
        name = "JetBrainsMono Nerd Font";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 12;
        desktop = 12;
        popups = 12;
        terminal = 12;
      };
    };
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "github.com" = {
        user = "git";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path;
      };
      "csl" = {
        user = "schwennesen";
        hostname = "best-linux.cs.wisc.edu";
        controlMaster = "auto";
        controlPath = "~/.ssh/persist_connections/%r@%h:%p";
        controlPersist = "yes";
      };
      "git.doit.wisc.edu" = {
        user = "git";
        hostname = "git.doit.wisc.edu";
        identitiesOnly = true;
        identityFile = osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path;
      };
      # Default config, which is going away soon
      "*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };

  home.file.".ssh/config" = {
    target = ".ssh/config_source";
    onChange = ''cat .ssh/config_source > .ssh/config && chmod 400 .ssh/config'';
  };

  home.packages = with pkgs; [
    zoxide
    sox
    rclone
  ];

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
