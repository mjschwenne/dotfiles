{
  appimageTools,
  fetchurl,
  ...
}:
appimageTools.wrapType2 {
  # or wrapType1
  name = "muffon-2.0.2";
  src = fetchurl {
    url = "https://github.com/staniel359/muffon/releases/download/v2.0.2/muffon-2.0.2-linux-x86_64.AppImage";
    hash = "sha256-bJfnYX5ZCAJNSGP4WJcDpVKX7yAqFYC7sqDIGQWwkTM=";
  };
  extraPkgs = pkgs: with pkgs; [];
}
