# Tailscale - VPN service and system tray
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.services.tailscaleDesktop;
in
{
  options.services.tailscaleDesktop = {
    enable = lib.mkEnableOption "Tailscale with system tray applet for desktop use";
  };

  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;

    home-manager.users.${user} = {
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
    };
  };
}
