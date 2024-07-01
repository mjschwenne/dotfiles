{
  stdenv,
  fetchFromGitHub,
}: {
  sddm-sugar = stdenv.mkDerivation rec {
    pname = "sddm-sugar-candy-theme";
    version = "a1fae51";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/sddm/themes
      cp -aR $src $out/share/sddm/themes/sugar-candy
    '';
    src = fetchFromGitHub {
      owner = "Kangie";
      repo = "sddm-sugar-candy";
      rev = "a1fae5159c8f7e44f0d8de124b14bae583edb5b8";
      sha256 = "p2d7I0UBP63baW/q9MexYJQcqSmZ0L5rkwK3n66gmqM=";
    };
  };
}
