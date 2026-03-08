# Auto-upgrade - automatically pull latest config and rebuild
# Great for machines you want to stay up-to-date without manual intervention
{ config, lib, hostname, ... }:

{
  system.autoUpgrade = {
    enable = true;

    # Pull from GitHub - change to your repo URL
    flake = "github:CRBroughton/nix-configuration#${hostname}";

    # Refresh flake inputs (like running `nix flake update` first)
    flags = [ "--refresh" ];

    # When to check for updates
    # Options: "daily", "weekly", "monthly", or cron syntax like "04:00"
    dates = "daily";

    # Allow rebooting if needed (kernel updates, etc.)
    # Set to false if you don't want automatic reboots
    allowReboot = false;

    # Only reboot during specific hours (if allowReboot = true)
    # rebootWindow = {
    #   lower = "02:00";
    #   upper = "06:00";
    # };
  };
}
