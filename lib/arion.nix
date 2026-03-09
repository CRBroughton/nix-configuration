# Arion helper functions for creating services with Tailscale sidecars
{ lib }:

{
  # Creates an arion project with tailscale sidecar pattern
  # Usage:
  #   mkTailscaleService {
  #     name = "freshrss";
  #     hostname = "freshrss";  # Tailscale hostname
  #     image = "lscr.io/linuxserver/freshrss:latest";
  #     environment = { PUID = "1000"; PGID = "1000"; };
  #     volumes = [ "${dataDir}/volume:/config" ];
  #   }
  mkTailscaleService =
    {
      name,
      hostname ? name,
      image,
      environment ? { },
      volumes ? [ ],
      dataDir ? "/etc/nixos/services/${name}",
      envFile ? null, # Optional .env file path (for TS_AUTHKEY on initial setup)
      extraServiceConfig ? { },
    }:
    {
      settings = {
        services = {
          # Tailscale sidecar for networking
          "tailscale-${name}" = {
            service = {
              image = "tailscale/tailscale:latest";
              container_name = "tailscale-${name}";
              inherit hostname;
              restart = "unless-stopped";
              environment = {
                TS_STATE_DIR = "/var/lib/tailscale";
                TS_SERVE_CONFIG = "/config/serve.json";
              };
              volumes = [
                "${dataDir}/tailscale:/var/lib/tailscale"
                "${dataDir}/serve.json:/config/serve.json:ro"
              ];
              capabilities = {
                NET_ADMIN = true;
                SYS_MODULE = true;
              };
              devices = [ "/dev/net/tun:/dev/net/tun" ];
            }
            // lib.optionalAttrs (envFile != null) {
              env_file = [ envFile ];
            };
          };

          # Main application
          "${name}" = {
            service = {
              inherit image;
              container_name = name;
              restart = "unless-stopped";
              network_mode = "service:tailscale-${name}";
              depends_on = [ "tailscale-${name}" ];
              environment = {
                TZ = "Etc/UTC";
              }
              // environment;
              inherit volumes;
            }
            // extraServiceConfig;
          };
        };
      };
    };
}
