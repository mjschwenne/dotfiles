{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchYarnDeps,
  yarnConfigHook,
  yarnBuildHook,
  writableTmpDirAsHomeHook,
  nodejs_22,
  # organice is frontend only: every one of these is inlined into the parcel
  # bundle at build time rather than read at runtime, so changing any of them
  # rebuilds the package. Nothing here is actually a secret -- whatever is set
  # ends up in JavaScript that every visitor downloads.
  dropboxClientId ? "",
  gitlabClientId ? "",
  gitlabSecret ? "",
  webdavUrl ? "",
}:

let
  # organice reads .env through dotenv during the build; there is no .env in
  # the repo (only .env.sample), so write the one we want before building.
  envFile = builtins.toFile "organice.env" ''
    REACT_APP_DROPBOX_CLIENT_ID=${dropboxClientId}
    REACT_APP_GITLAB_CLIENT_ID=${gitlabClientId}
    REACT_APP_GITLAB_SECRET=${gitlabSecret}
    REACT_APP_WEBDAV_URL=${webdavUrl}
  '';
in
stdenv.mkDerivation (finalAttrs: {
  pname = "organice";
  version = "0.2.0-unstable-2026-06-15";

  # jesseylin's fork rather than 200ok-ch/organice: it carries the parcel
  # migration (upstream is still on create-react-app) and the nix build.
  src = fetchFromGitHub {
    owner = "jesseylin";
    repo = "organice";
    rev = "5fdf4af0d3a69ad8c27e3a91821f870b06df3925";
    hash = "sha256-Fi3Y4dYjGgtCfiaeiHD1EX+6p4mBfVWYWtoeW+Qtzig=";
  };

  yarnOfflineCache = fetchYarnDeps {
    yarnLock = "${finalAttrs.src}/yarn.lock";
    hash = "sha256-PVxdwevmg3RDmUnh4/AW/sOC7czcfmo+Q0eBv2FViAw=";
  };

  nativeBuildInputs = [
    writableTmpDirAsHomeHook # parcel and yarn both want a writable $HOME
    yarnConfigHook
    yarnBuildHook
    nodejs_22
  ];

  # package.json pins "node": "^20.17.0", which nixpkgs dropped at Node 20's
  # EOL. Parcel builds fine on 22; this stops yarn refusing to install.
  YARN_IGNORE_ENGINES = "1";

  # `yarn build` shells out to bin/compile_search_parser.sh.
  postPatch = ''
    patchShebangs bin
  '';

  preBuild = ''
    cp ${envFile} .env
  '';

  # parcel build writes to dist/; the result is plain static files with no
  # server side whatsoever.
  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/organice
    cp -r dist/. $out/share/organice/

    runHook postInstall
  '';

  meta = {
    description = "Browser-based outliner for Org mode files";
    homepage = "https://organice.200ok.ch";
    license = lib.licenses.agpl3Only;
    platforms = lib.platforms.all;
  };
})
