{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  wrapGAppsHook,
  gtk3,
  librsvg,
  libdbusmenu,
  libdbusmenu-gtk3,
  withWayland ? false,
  gtk-layer-shell,
}:
rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "git-2024-01-06";

  src = fetchFromGitHub {
    owner = "ralismark";
    repo = "eww";
    rev = "5b507c813c79be42b174f477b7acd2c95d58f09f";
    hash = "sha256-oTxEbleVjtagYqFAb0rcoqvDcYcmiTgKCF9mk11ztSo=";
  };

  cargoHash = "sha256-RcTiLG9CdQQG6+pOreU7NJCxg0DjQ7ogYLI6Hvms8Ik=";

  nativeBuildInputs = [pkg-config wrapGAppsHook];

  buildInputs = [gtk3 librsvg libdbusmenu libdbusmenu-gtk3] ++ lib.optional withWayland gtk-layer-shell;

  buildNoDefaultFeatures = true;
  buildFeatures = [
    (
      if withWayland
      then "wayland"
      else "x11"
    )
  ];

  cargoBuildFlags = ["--bin" "eww"];

  cargoTestFlags = cargoBuildFlags;

  # requires unstable rust features
  RUSTC_BOOTSTRAP = 1;
}
