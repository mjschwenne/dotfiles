{pkgs, ...}: {
  home = {
    username = "mjs";
    homeDirectory = "/home/mjs";
    sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
    };
  };

  imports = [
    ./common.nix
    ./graphical.nix
    ./ui
    ./applications
    ./applications/wonderdraft
    ./applications/dungeondraft
    ./applications/r
    ./editors/emacs
  ];

  home.packages = with pkgs; [
    # Graphics
    (blender.override {
      cudaSupport = true;
    })
    krita

    # LLM
    ollama
    oterm
  ];
}
