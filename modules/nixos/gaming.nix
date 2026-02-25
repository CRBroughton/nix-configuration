{ config, pkgs, ... }:

{
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  programs.gamemode.enable = true;
  programs.gamescope.enable = true;

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;
}
