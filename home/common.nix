{
  osConfig,
  pkgs,
  ...
} @ inputs: {
  home = {
    username = "mjs";
    homeDirectory = "/home/mjs";
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        user = "git";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = osConfig.sops.secrets."ssh/${osConfig.networking.hostName}/ssh/key".path;
      };
      "git.doit.wisc.edu" = {
        user = "git";
        hostname = "git.doit.wisc.edu";
        identitiesOnly = true;
        identityFile = osConfig.sops.secrets."ssh/${osConfig.networking.hostName}/ssh/key".path;
      };
    };
  };

  home.file.".ssh/config" = {
    target = ".ssh/config_source";
    onChange = ''cat .ssh/config_source > .ssh/config && chmod 400 .ssh/config'';
  };

  imports = [
    ./cli

    ./editors
  ];

  home.packages = with pkgs; [zoxide sox];

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
