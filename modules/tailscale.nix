{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tailscale
    tailscalesd
    tailscale-systray
  ];

  # Start tailscaled as a user service
  # Note: On non-NixOS systems, tailscaled typically needs root privileges
  # You may need to install tailscale via your distro's package manager for the system service
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
