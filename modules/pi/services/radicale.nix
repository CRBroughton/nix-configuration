{ config, lib, ... }:

let
  cfg = config.modules.pi.services.radicale;
  dataDir = "/var/lib/radicale";
in
{
  options.modules.pi.services.radicale = {
    enable = lib.mkEnableOption "Radicale CalDAV/CardDAV server";
    port = lib.mkOption {
      type = lib.types.port;
      default = 5232;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir} 0755 root root -"
    ];

    virtualisation.oci-containers.containers.radicale = {
      image = "tomsquest/docker-radicale:latest";
      autoStart = true;
      volumes = [
        "${dataDir}:/data"
      ];
      environment = {
        TZ = "Europe/London";
      };
      extraOptions = [ "--network=host" ];
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];
  };
}
