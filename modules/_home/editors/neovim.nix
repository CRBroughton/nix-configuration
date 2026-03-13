{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.editors.neovim;
in
{
  options.editors.neovim = {
    enable = lib.mkEnableOption "Neovim with external config";
  };

  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      defaultEditor = false;
    };

    xdg.configFile."nvim".source = ../../../config/neovim;

    home.packages = with pkgs; [
      tree-sitter
    ];
  };
}
