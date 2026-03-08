{ config, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    settings = {
      shell-integration = "fish";
      command = "${pkgs.fish}/bin/fish";
      font-family = "FiraCode Nerd Font Mono";
      font-size = 14;
      font-feature = [ "calt" "liga" ];
      font-style = "SemiBold";
      background = "#171717";
      foreground = "#dedacf";
      background-opacity = 0.75;
      background-blur-radius = 80;
    };
  };
}
