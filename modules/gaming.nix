# Gaming - Steam, Gamemode, Lutris, graphics support
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.gaming;
in
{
  options.modules.gaming = {
    enable = lib.mkEnableOption "gaming support with Steam, Gamemode, Gamescope, and Lutris";
  };

  config = lib.mkIf cfg.enable {
    # ============================================
    # NixOS (system level)
    # ============================================

    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    programs.gamemode.enable = true;
    programs.gamescope.enable = true;

    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = true;
    hardware.graphics.extraPackages = with pkgs; [
      lsfg-vk
    ];

    # ============================================
    # Home-manager (user level)
    # ============================================

    home-manager.users.${user} = {
      home.packages = with pkgs; [
        lutris
        wine-wayland
        winetricks
        lsfg-vk
        lsfg-vk-ui
        mangohud
      ];
    };
  };
}
