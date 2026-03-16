# Server Auto-Upgrade - Git pull then rebuild (for /etc/nixos servers)
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.server.autoUpgrade;
in
{
  options.modules.server.autoUpgrade = {
    enable = lib.mkEnableOption "automatic NixOS upgrades with git pull";
  };

  config = lib.mkIf cfg.enable {
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

    systemd.services."custom-nixos-upgrade-git-pull" = {
      description = "Pull latest NixOS configuration from git";
      before = [ "nixos-upgrade.service" ];
      wantedBy = [ "nixos-upgrade.service" ];
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/etc/nixos";
        ExecStart = "${pkgs.git}/bin/git pull --ff-only";
      };
    };
  };
}
