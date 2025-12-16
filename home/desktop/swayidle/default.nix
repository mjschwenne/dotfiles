{pkgs, ...}: {
  services.swayidle = {
    enable = true;
    systemdTarget = "graphical-session.target";
    events = {
      before-sleep = "swaylock";
    };
    timeouts = [
      {
        timeout = 300;
        command = "swaylock";
      }
      {
        timeout = 600;
        command = "${pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
}
