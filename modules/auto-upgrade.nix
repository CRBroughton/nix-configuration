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
    enable = lib.mkEnableOption "automatic NixOS upgrades by pulling from GitHub and rebuilding daily";
  };

  config = lib.mkIf cfg.enable {
    system.autoUpgrade = {
      enable = true;
      flake = "github:CRBroughton/nix-configuration/nixos-migration#${hostname}";
      flags = [ "--refresh" ];
      dates = "daily";
      allowReboot = false;
    };
  };
}
