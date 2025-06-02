{
  config,
  pkgs,
  ...
}: {
  # Media server networking
  # This will configure a split DNS using NextDNS
  # to direct traffic between tailscale and protonvpn

  sops.secrets = {
    "nextdns/proton".owner = "root";
    "${config.networking.hostName}/protonvpn".owner = "root";
    "${config.networking.hostName}/tailscale".owner = "mjs";
  };

  networking = {
    nameservers = ["127.0.0.1" "::1"];
    wg-quick.interfaces.wg-proton = {
      address = ["10.2.0.2/32"];
      dns = ["10.2.0.1"];
      privateKeyFile = config.sops.secrets."${config.networking.hostName}/protonvpn".path;
      postUp = ''
        ${pkgs.iproute2}/bin/ip route add 100.64.0.0/10 dev tailscale0
        ${pkgs.procps}/bin/sysctl -w net.ipv6.conf.wg-proton.disable_ipv6=1
      '';
      postDown = ''
        ${pkgs.iproute2}/bin/ip route del 100.64.0.0/10 dev tailscale0
        ${pkgs.procps}/bin/sysctl -w net.ipv6.conf.wg-proton.disable_ipv6=0
      '';
      peers = [
        {
          publicKey = "Gme1WiCEJZZusmIlg1aqItLaxDhdMl51xypUy43oMmc=";
          allowedIPs = ["0.0.0.0/0"];
          endpoint = "154.47.25.206:51820";
        }
      ];
    };
  };
  services = {
    nextdns = {
      enable = true;
      arguments = [
        "-config-file"
        ''${config.sops.secrets."nextdns/proton".path}''
      ];
    };
    tailscale = {
      enable = true;
      extraUpFlags = ["--ssh" "--accept-dns=false"];
      authKeyFile = config.sops.secrets."${config.networking.hostName}/tailscale".path;
    };
  };
  # Make sure that wg-proton is started after tailscale, so the
  # tailscale0 network interface exists
  systemd.services = {
    mjs-tailscale-up = {
      enable = true;
      after = ["tailscale.service" "sys-subsystem-net-devices.tailscale0.device"];
      wantedBy = ["multi-user.target"];
      serviceConfig.Type = "oneshot";
      script = ''
        timeout 60s ${pkgs.bash}/bin/bash -c "until ${pkgs.tailscale}/bin/tailscale status --peers=false; do sleep 1; done"
      '';
    };
    wg-quick-wg-proton.after = ["mjs-tailscale-up.service"];
  };
}
