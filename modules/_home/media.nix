{ config, lib, pkgs, ... }:

let cfg = config.media; in
{
  options.media = {
    enable = lib.mkEnableOption "media tools (qpwgraph, picard, mumble)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      qpwgraph
      picard
      mumble
    ];
  };
}
