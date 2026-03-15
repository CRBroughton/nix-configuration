# Terminal - Ghostty terminal emulator
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.terminal;
in
{
  options.modules.terminal = {
    enable = lib.mkEnableOption "Ghostty terminal emulator";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.ghostty = {
        enable = true;
        settings = {
          shell-integration = "fish";
          command = "${pkgs.fish}/bin/fish";
          font-family = "FiraCode Nerd Font Mono";
          font-size = 14;
          font-feature = [
            "calt"
            "liga"
          ];
          font-style = "SemiBold";
          background = "#171717";
          foreground = "#dedacf";
          background-opacity = 0.75;
          background-blur-radius = 80;
        };
      };
    };
  };
}
