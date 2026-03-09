# Tailscale - VPN service and system tray
{
  pkgs,
  user,
  ...
}:

{
  # ============================================
  # NixOS (system level)
  # ============================================

  services.tailscale.enable = true;

  # ============================================
  # Home-manager (user level)
  # ============================================

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
}
