{ lib, unzip, stdenv, autoPatchelfHook
, xorg, libpulseaudio, alsaLib, libGL, libkrb5
}:
let
  version = "1.0.4.7";
  src = ./Dungeondraft-1.0.4.7-Linux64.zip;
in
stdenv.mkDerivation {
  name = "dungeondraft-${version}";

  inherit src;

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/opt/Dungeondraft/

    unzip ${src} -d $out/opt/Dungeondraft/
    chmod +x $out/opt/Dungeondraft/Dungeondraft.x86_64

    mkdir -p $out/share/icons
    ln -s $out/opt/Dungeondraft/Dungeondraft.png $out/share/icons/dungeondraft.png

    mkdir -p $out/bin
    ln -s $out/opt/Dungeondraft/Dungeondraft.x86_64 $out/bin/dungeondraft
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
    libkrb5
  ];

  meta = with lib; {
    platforms = [ "x86_64-linux" ];
  };
}

