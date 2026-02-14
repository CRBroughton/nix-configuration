{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    # Don't install tailscale CLI from Nix to avoid version mismatch with system daemon
    # Use the system-installed tailscale from pacman instead
    tailscale-systray
  ];

  # Check if tailscale is installed and show install command if not
  home.activation.checkTailscale = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if ! systemctl is-active --quiet tailscaled 2>/dev/null; then
      echo ""
      echo "⚠️  Tailscale service not found. To install, run:"
      echo "   curl -fsSL https://tailscale.com/install.sh | sh"
      echo ""
    fi
  '';

  # Start tailscale system tray
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
