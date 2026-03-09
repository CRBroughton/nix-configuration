# Server Auto-Upgrade - Git pull then rebuild (for /etc/nixos servers)
{ pkgs, ... }:

{
  # Automatic system updates from local /etc/nixos
  system.autoUpgrade = {
    enable = true;
    flake = "/etc/nixos";
    flags = [
      "--update-input"
      "nixpkgs"
    ];
    dates = "04:00";
    allowReboot = false;
  };

  # Pull latest configuration before auto-upgrade
  systemd.services."nixos-upgrade-git-pull" = {
    description = "Pull latest NixOS configuration from git";
    before = [ "nixos-upgrade.service" ];
    wantedBy = [ "nixos-upgrade.service" ];
    serviceConfig = {
      Type = "oneshot";
      WorkingDirectory = "/etc/nixos";
      ExecStart = "${pkgs.git}/bin/git pull --ff-only";
    };
  };
}
