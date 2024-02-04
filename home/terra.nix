{pkgs, ...}: let
  patched_ollama = pkgs.ollama.overrideAttrs (old: rec {
    version = "0.1.17";
    postPatch = ''
      substituteInPlace llm/llama.go \
        --subst-var-by llamaCppServer "${pkgs.llama-cpp}/bin/llama-server"
      substituteInPlace server/routes_test.go --replace "0.0.0" "${version}"
    '';
  });
in {
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
    ./applications/obs
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
    patched_ollama
    oterm
  ];
}
