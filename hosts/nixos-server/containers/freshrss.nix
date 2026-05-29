{ config, ... }:
{
  containers.freshrss = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.1";
    localAddress = "192.168.100.2";

    forwardPorts = [
      {
        hostPort = 9101;
        containerPort = 9100;
      }
    ];

    bindMounts."/var/lib/freshrss" = {
      hostPath = "/etc/nixos/services/freshrss/volume";
      isReadOnly = false;
    };

    bindMounts."/var/lib/tailscale" = {
      hostPath = "/etc/nixos/services/freshrss/tailscale";
      isReadOnly = false;
    };

    bindMounts."/run/secrets/freshrss_password" = {
      hostPath = config.age.secrets.freshrss_password.path;
      isReadOnly = true;
    };

    bindMounts."/run/secrets/ts_authkey" = {
      hostPath = config.age.secrets.ts_authkey.path;
      isReadOnly = true;
    };

    enableTun = true;

    config =
      { pkgs, ... }:
      {
        imports = [ ../../../../modules/containers/common.nix ];

        services.freshrss = {
          enable = true;
          defaultUser = "admin";
          passwordFile = "/run/secrets/freshrss_password";
          baseUrl = "https://freshrss.tail538465.ts.net";
        };

        services.tailscale = {
          enable = true;
          authKeyFile = "/run/secrets/ts_authkey";
          extraUpFlags = [ "--hostname=freshrss" ];
        };

        systemd.services.tailscale-serve = {
          wantedBy = [ "multi-user.target" ];
          after = [
            "tailscaled.service"
            "tailscaled-autoconnect.service"
          ];
          serviceConfig.Type = "oneshot";
          script = ''
            ${pkgs.tailscale}/bin/tailscale serve --bg https / http://127.0.0.1:80
          '';
        };
      };
  };
}
