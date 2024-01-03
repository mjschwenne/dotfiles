# https://nixos.org/manual/nix/stable/package-management/garbage-collector-roots for how to pin something in the nix store
{
  lib,
  unzip,
  stdenv,
  autoPatchelfHook,
  xorg,
  libpulseaudio,
  alsaLib,
  libGL,
}: let
  version = "1.1.7.3";
  src = ./Wonderdraft-1.1.7.3-Linux64.zip;
in
  stdenv.mkDerivation {
    name = "wonderdraft-${version}";

    inherit src;

    unpackPhase = "true";

    installPhase = ''
      mkdir -p $out/opt/Wonderdraft/

      unzip ${src} -d $out/opt/Wonderdraft/
      chmod +x $out/opt/Wonderdraft/Wonderdraft.x86_64

      mkdir -p $out/share/icons
      ln -s $out/opt/Wonderdraft/Wonderdraft.png $out/share/icons/wonderdraft.png

      mkdir -p $out/bin
      ln -s $out/opt/Wonderdraft/Wonderdraft.x86_64 $out/bin/wonderdraft
    '';

    nativeBuildInputs = [
      autoPatchelfHook
      unzip
    ];

    buildInputs = [
      xorg.libX11
      xorg.libXcursor
      xorg.libXinerama
      xorg.libXrandr
      xorg.libXi
      alsaLib
      libpulseaudio
      libGL
    ];

    meta = with lib; {
      platforms = ["x86_64-linux"];
    };
  }
