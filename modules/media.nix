# Media - qpwgraph, picard, mumble
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
    enable = lib.mkEnableOption "media tools (qpwgraph, picard, mumble)";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = with pkgs; [
        qpwgraph
        picard
        mumble
      ];
    };
  };
}
