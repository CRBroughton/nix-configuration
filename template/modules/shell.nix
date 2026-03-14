# Shell - enable with: shell.enable = true
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.shell;
in
{
  options.shell = {
    enable = lib.mkEnableOption "fish shell with starship prompt and common CLI tools";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.fish = {
        enable = true;
        shellAliases = {
          ls = "eza";
          cat = "bat";
        };
      };

      programs.starship = {
        enable = true;
        enableFishIntegration = true;
      };

      home.packages = with pkgs; [
        eza
        bat
        ripgrep
        btop
      ];
    };
  };
}
