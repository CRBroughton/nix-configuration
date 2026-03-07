{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tailscale-systray
  ];

  systemd.user.services.tailscale-systray = {
    Unit = {
      Description = "Tailscale System Tray";
      After = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.tailscale-systray}/bin/tailscale-systray";
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}