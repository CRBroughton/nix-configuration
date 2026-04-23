# Media - OBS Studio, qpwgraph, picard, mumble
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.media;
in
{
  options.modules.media = {
    enable = lib.mkEnableOption "media tools (obs-studio, qpwgraph, picard, mumble)";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = with pkgs; [
        obs-studio
        qpwgraph
        picard
        mumble
      ];
    };
  };
}
