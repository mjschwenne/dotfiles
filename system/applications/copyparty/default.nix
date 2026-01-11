{ pkgs, config, ... }:
{
  services.copyparty = {
    enable = true;
    package = pkgs.copyparty-full;
    settings = {
      i = "0.0.0.0";
      p = [ 3923 ];
      rproxy = -1;
    };

    accounts = {
      mjs.passwordFile = config.sops.secrets."copyparty/mjs".path;
    };

    volumes = {
      "/" = {
        path = "/var/lib/copyparty/data";
        access = {
          "rwmd." = [ "mjs" ];
        };
        flags = {
          e2d = true;
          dots = true;
          dotsrch = true;
        };
      };
    };
  };
}
