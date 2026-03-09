# FreshRSS - RSS feed reader with Tailscale sidecar
{
  lib,
  ...
}:

let
  arionLib = import ../../../lib/arion.nix { inherit lib; };
  dataDir = "/etc/nixos/services/freshrss";
in
{
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
}
