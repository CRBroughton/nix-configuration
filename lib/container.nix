{
  pkgs,
  config,
}:

{
  mkTailscaleService =
    {
      name,
      hostname ? name,
      image,
      environment ? { },
      volumes ? [ ],
      dataDir ? "/mnt/services/${name}",
      tsSecretFile ? config.age.secrets.ts_authkey.path,
      servePort ? 80,
      extraOptions ? [ ],
    }:
    let
      serveJson = pkgs.writeText "tailscale-serve-${name}.json" (
        builtins.toJSON {
          TCP."443".HTTPS = true;
          Web."\${TS_CERT_DOMAIN}:443".Handlers."/".Proxy = "http://127.0.0.1:${toString servePort}";
        }
      );
    in
    {
      "tailscale-${name}" = {
        image = "tailscale/tailscale:latest";
        autoStart = true;
        environment = {
          TS_HOSTNAME = hostname;
          TS_STATE_DIR = "/var/lib/tailscale";
          TS_SERVE_CONFIG = "/config/serve.json";
        };
        environmentFiles = [ tsSecretFile ];
        volumes = [
          "${dataDir}/tailscale:/var/lib/tailscale"
          "${serveJson}:/config/serve.json:ro"
        ];
        extraOptions = [
          "--network=container:${name}"
          "--cap-add=NET_ADMIN"
          "--cap-add=SYS_MODULE"
          "--device=/dev/net/tun"
        ];
      };

      "${name}" = {
        inherit image;
        autoStart = true;
        environment = {
          TZ = "Etc/UTC";
          PUID = "1000";
          PGID = "1000";
        }
        // environment;
        inherit volumes;
        extraOptions = [ "--network=homelab" ] ++ extraOptions;
      };
    };
}
