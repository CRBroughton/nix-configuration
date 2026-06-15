{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.freshrss;
  dataDir = "/mnt/services/freshrss";
  containerLib = import ../../lib/container.nix { inherit config pkgs; };
in
{
  options.modules.freshrss = {
    enable = lib.mkEnableOption "FreshRSS RSS feed reader";
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir}           0755 root root -"
      "d ${dataDir}/tailscale 0755 root root -"
      "d ${dataDir}/volume    0755 root root -"
    ];

    virtualisation.oci-containers.containers = containerLib.mkTailscaleService {
      name = "freshrss";
      hostname = "freshrss";
      image = "lscr.io/linuxserver/freshrss:latest";
      volumes = [
        "${dataDir}/volume:/config"
      ];
    };

    systemd.services."podman-tailscale-freshrss" = {
      after = [ "podman-freshrss.service" ];
      requires = [ "podman-freshrss.service" ];
    };
  };
}
