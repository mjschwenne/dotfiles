{
  stdenv,
  lib,
  autoPatchelfHook,
  dpkg,
  libXcursor,
  libXinerama,
  libXext,
  libXrandr,
  libXi,
  libGL,
  zlib,
  libkrb5,
  udev,
  makeWrapper,
  zenity,
}:
stdenv.mkDerivation {
  pname = "dungeondraft";
  version = "1.1.0.3";

  src = /nix/store/s0rhwf1k8gwgafr95p0fgk14ybqh839x-Dungeondraft-1.1.0.3-Linux64.deb;

  dontBuild = true;
  dontConfigure = true;

  unpackPhase = ''
    dpkg-deb -x $src .
  '';

  buildInputs = [
    libXcursor
    libXinerama
    libXext
    libXrandr
    libXi
    libGL
    zlib
    libkrb5
  ];

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/opt/Dungeondraft
    mv opt/Dungeondraft/* $out/opt/Dungeondraft
    # Can't use wrapProgram because godot seems to load data files based upon executable name
    makeWrapper $out/opt/Dungeondraft/Dungeondraft.x86_64 $out/opt/Dungeondraft/Dungeondraft.x86_64.wrapped \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [udev]} \
      --prefix PATH : ${lib.makeBinPath [zenity]}
    mkdir -p $out/share/applications
    mv usr/share/applications/* $out/share/applications
    sed -i "s|Exec=/opt/Dungeondraft/Dungeondraft.x86_64|Exec=$out/opt/Dungeondraft/Dungeondraft.x86_64.wrapped|g" $out/share/applications/Dungeondraft.desktop
    sed -i "s|Path=/opt/Dungeondraft|Path=$out/opt/Dungeondraft|g" $out/share/applications/Dungeondraft.desktop
    sed -i "s|Icon=/opt/Dungeondraft/Dungeondraft.png|Icon=$out/opt/Dungeondraft/Dungeondraft.png|g" $out/share/applications/Dungeondraft.desktop
  '';
}
