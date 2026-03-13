{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.git;
in
{
  options.git = {
    enable = lib.mkEnableOption "git with lazygit";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      settings = {
        init.defaultBranch = "master";
      };
    };

    home.packages = with pkgs; [
      lazygit
    ];
  };
}
