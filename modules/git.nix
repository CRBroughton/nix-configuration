# Git - git with lazygit
{
  config,
  lib,
  pkgs,
  inputs,
  user,
  ...
}:

let
  cfg = config.modules.git;
in
{
  options.modules.git = {
    enable = lib.mkEnableOption "git, gh & lazygit";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.git = {
        enable = true;
        settings = {
          init.defaultBranch = "master";
        };
      };

      home.packages = with pkgs; [
        lazygit
        inputs.jjui.packages.${pkgs.system}.default
        gh
        gource
      ];
    };
  };
}
