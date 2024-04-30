{
  osConfig,
  pkgs,
  ...
}: let
  oterm_updated = pkgs.oterm.overrideAttrs (old: rec {
    version = "0.2.4";
    src = pkgs.fetchFromGitHub {
      owner = "ggozad";
      repo = "oterm";
      rev = "a0167f4e6f7bc8057cbcaaa688c7a71080ec6f64";
      hash = "sha256-p0ns+8qmcyX4gcg0CfYdDMn1Ie0atVBuQbVQoDRQ9+c=";
    };
    propagatedBuildInputs = with pkgs.python3Packages; [
      (textual.overrideAttrs rec {
        version = "0.53.1";
        src = pkgs.fetchFromGitHub {
          owner = "Textualize";
          repo = "textual";
          rev = "refs/tags/v0.53.1";
          hash = "sha256-73qEogHe69B66r4EJOj2RAP95O5z7v/UYARTIEPxrcA=";
        };
      })
      typer
      python-dotenv
      httpx
      aiosql
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
    # oterm_updated
  ];
}
