{pkgs, ...}: let
  patched_ollama = pkgs.ollama.overrideAttrs (old: rec {
    version = "0.1.17";
    postPatch = ''
      substituteInPlace llm/llama.go \
        --subst-var-by llamaCppServer "${pkgs.llama-cpp}/bin/llama-server"
      substituteInPlace server/routes_test.go --replace "0.0.0" "${version}"
    '';
  });
  # updated_textual = pkgs.callPackage ./python-pkgs/textual.nix {
  #   inherit (pkgs) tree-sitter;
  #   inherit (pkgs.python3Packages) buildPythonPackage;
  #   jinja2 = pkgs.jinja2-cli;
  #   inherit (pkgs.python311Packages) importlib-metadata markdown-it-py poetry-core pytest-aiohttp pytestCheckHook rich syrupy time-machine typing-extensions;
  # };
  oterm_updated = pkgs.oterm.overrideAttrs (old: rec {
    version = "0.2.4";
    src = pkgs.fetchFromGitHub {
      owner = "ggozad";
      repo = "oterm";
      rev = "refs/tags/${version}";
      hash = "sha256-bMfRMJPf62S0UFDOUnZAHpP8DEjBj2GAJvQ2T7AAAX0=";
    };
    propagatedBuildInputs = with pkgs.python3Packages; [
      (textual.overrideAttrs rec {
        version = "0.50.1";
        src = pkgs.fetchFromGitHub {
          owner = "Textualize";
          repo = "textual";
          rev = "refs/tags/v0.50.1";
          hash = "sha256-jt3HX8p5KVE3/ROKaQpU+jA4bjqW+idjynTdgvWrtYI=";
        };
      })
      typer
      python-dotenv
      httpx
      (aiosql.overrideAttrs rec {
        version = "9.3";
        src = pkgs.fetchFromGitHub {
          owner = "nackjicholson";
          repo = "aiosql";
          rev = "refs/tags/9.3";
          hash = "sha256-7bCJykE+7/eA1h4L5MyH/zVPZVMt7cNLXZSWq+8mPtY=";
        };
      })
      aiosqlite
      pyperclip
      packaging
      rich-pixels
      pillow
      aiohttp
    ];
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
    ./applications/rembg
    ./editors/emacs
  ];

  home.packages = with pkgs; [
    # Graphics
    (blender.override {
      cudaSupport = true;
    })
    krita

    # Audio
    audacity
    ardour

    # LLM
    # patched_ollama
    ollama
    oterm_updated
  ];
}
