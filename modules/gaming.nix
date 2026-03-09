# Gaming - Steam, Gamemode, Lutris, graphics support
{
  pkgs,
  user,
  ...
}:

{
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

  # ============================================
  # Home-manager (user level)
  # ============================================

  home-manager.users.${user} = {
    home.packages = with pkgs; [
      lutris
    ];
  };
}
