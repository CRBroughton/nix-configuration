# FreshRSS - RSS feed reader with Tailscale sidecar
{ config, lib, ... }:

let
  cfg = config.modules.server.services.freshrss;
  arionLib = import ../../../lib/arion.nix { inherit lib; };
  dataDir = "/etc/nixos/services/freshrss";
in
{
  options.modules.server.services.freshrss = {
    enable = lib.mkEnableOption "FreshRSS RSS reader (Arion/Docker)";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.arion.projects.freshrss = arionLib.mkTailscaleService {
      name = "freshrss";
      image = "lscr.io/linuxserver/freshrss:latest";
      environment = {
        PUID = "1000";
        PGID = "1000";
      };
      volumes = [
        "${dataDir}/volume:/config"
      ];
    };
  };
}
