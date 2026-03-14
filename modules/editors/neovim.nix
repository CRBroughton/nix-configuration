# Neovim with external config
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = {
    enable = lib.mkEnableOption "Neovim with external config";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.neovim = {
        enable = true;
        defaultEditor = false;
      };

      xdg.configFile."nvim".source = ../../config/neovim;

      home.packages = with pkgs; [
        tree-sitter
      ];
    };
  };
}
