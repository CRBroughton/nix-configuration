# Auto-upgrade - automatically pull latest config and rebuild
{
  config,
  lib,
  hostname,
  ...
}:

let
  cfg = config.modules.autoUpgrade;
in
{
  options.modules.autoUpgrade = {
    enable = lib.mkEnableOption "automatic NixOS upgrades by pulling from GitHub and rebuilding";
    dates = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "When to run upgrades. Accepts systemd calendar expressions (e.g. \"daily\", \"hourly\", \"*:0/15\").";
    };
  };

  config = lib.mkIf cfg.enable {
    system.autoUpgrade = {
      enable = true;
      flake = "github:CRBroughton/nix-configuration/master#${hostname}";
      flags = [ "--refresh" ];
      inherit (cfg) dates;
      allowReboot = false;
    };
  };
}
