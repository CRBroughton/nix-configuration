{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.syncserver;
  dataDir = "/mnt/services/syncserver";
  containerLib = import ../../lib/container.nix { inherit config pkgs; };
  containers = containerLib.mkTailscaleService {
    name = "syncserver";
    image = "mozilla/syncserver:latest";
    servePort = 5000;
    environment = {
      TZ = "Europe/London";
    };
    volumes = [
      "${dataDir}/data:/data"
      "${dataDir}/syncserver.ini:/app/syncserver.ini:ro"
    ];
  };
in
{
  options.modules.syncserver = {
    enable = lib.mkEnableOption "Firefox Sync Server";
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir}           0755 root root -"
      "d ${dataDir}/tailscale 0755 root root -"
      "d ${dataDir}/data      0755 root root -"
    ];

    virtualisation.oci-containers.containers = containers // {
      syncserver = containers.syncserver // {
        entrypoint = "/usr/local/bin/gunicorn";
        cmd = [
          "--paste"
          "/app/syncserver.ini"
        ];
      };
    };

    systemd.services."podman-tailscale-syncserver" = {
      after = [ "podman-syncserver.service" ];
      requires = [ "podman-syncserver.service" ];
    };
  };
}
