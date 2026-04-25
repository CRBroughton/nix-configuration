# Tailscale - VPN service and system tray
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.tailscale;
in
{
  options.modules.tailscale = {
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
          After = [ "graphical-session-pre.target" "tray.target" ];
          Requires = [ "tray.target" ];
        };
        Service = {
          ExecStartPre = "${pkgs.coreutils}/bin/sleep 3";
          ExecStart = "${pkgs.tailscale-systray}/bin/tailscale-systray";
          Restart = "on-failure";
          RestartSec = "5s";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
