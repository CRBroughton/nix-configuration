# Converting Docker Compose Files to NixOS oci-containers Modules

This guide is for converting existing `compose.yaml` files in the
`services/` directory into declarative NixOS modules using
`virtualisation.oci-containers`.

---

## Repository Context

- Config repo: `https://github.com/CRBroughton/nix-configuration`
- Service modules live in: `modules/services/`
- Shared container helper: `lib/container.nix`
- Service data lives at: `/mnt/services/<name>/`
- Secrets use agenix: `secrets/nixos-server/ts_authkey.age` (single shared reusable key)
- Options namespace: `options.modules.<name>`

---

## Key Files

### `lib/container.nix`

Shared helper that generates the container pair (app + Tailscale sidecar).
Accepts `{ pkgs, config }`.

```nix
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
```

Key defaults (can be overridden per service):
- `servePort` — internal port Tailscale proxies to, defaults to `80`
- `tsSecretFile` — path to the agenix-decrypted auth key, defaults to `config.age.secrets.ts_authkey.path`
- `PUID` / `PGID` — `1000`, matching most linuxserver.io images
- `TZ` — `Etc/UTC`
- `serve.json` — generated from Nix store, no manual file needed on the server

### `modules/storage.nix`

Ensures the base data directory exists. Import once in the server host.

```nix
{ ... }:
{
  systemd.tmpfiles.rules = [
    "d /mnt/services 0755 root root -"
  ];
}
```

### `hosts/nixos-server/default.nix`

The `homelab` podman network must exist before containers start. A systemd
oneshot creates it declaratively:

```nix
systemd.services.podman-create-homelab-network = {
  description = "Create podman homelab network";
  wantedBy = [ "multi-user.target" ];
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
    ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.podman}/bin/podman network inspect homelab &>/dev/null || ${pkgs.podman}/bin/podman network create homelab'";
  };
};
```

---

## agenix Secret Format

The `ts_authkey.age` secret **must** be in `KEY=VALUE` format — podman's
`--env-file` requires this. A raw key value will not be picked up by the
container and Tailscale will fall back to interactive auth.

Correct format inside the secret:
```
TS_AUTHKEY=tskey-auth-<key>
```

The auth key itself must be **reusable**. One-time keys are consumed on first
registration and will fail for all subsequent container starts or rebuilds.
Generate the key in the Tailscale admin console with the Reusable option
enabled.

To edit the secret:
```bash
cd secrets && nix run github:ryantm/agenix -- -e nixos-server/ts_authkey.age
```

---

## Module Template

Every service follows this exact pattern. Copy and fill in the blanks:

```nix
# modules/services/<name>.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.modules.<name>;
  dataDir = "/mnt/services/<name>";
  containerLib = import ../../lib/container.nix { inherit config pkgs; };
in
{
  options.modules.<name> = {
    enable = lib.mkEnableOption "<human readable description>";
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir}           0755 root root -"
      "d ${dataDir}/tailscale 0755 root root -"
      # one entry per volume directory
    ];

    virtualisation.oci-containers.containers =
      containerLib.mkTailscaleService {
        name = "<name>";
        image = "<image>";
        volumes = [
          # map from compose volumes: section
          # replace leading ./ with ${dataDir}/
        ];
      };

    systemd.services."podman-tailscale-<name>" = {
      after = [ "podman-<name>.service" ];
      requires = [ "podman-<name>.service" ];
    };
  };
}
```

Only specify what differs from the defaults:
- `environment` — only if the service needs env vars beyond `TZ`/`PUID`/`PGID`
- `servePort` — only if the app does not listen on port `80`
- `hostname` — only if it differs from `name`
- `tsSecretFile` — never, the default covers all server services
- `PUID` / `PGID` — only if the image uses non-standard IDs

If the service needs a port exposed on the local network (in addition to
Tailscale Serve), add a `port` option:

```nix
options.modules.<name>.port = lib.mkOption {
  type = lib.types.port;
  default = <PORT>;
  description = "Host port to expose <name> on";
};

# then in the container definition:
ports = [ "${toString cfg.port}:<CONTAINER_PORT>" ];
```

---

## Conversion Rules

### 1. Service name

Use the main app container name from compose (not the tailscale sidecar).

```yaml
# compose
services:
  tailscale-calibre:        # ignore
  calibre-web-automated:    # this becomes name = "calibre-web-automated"
```

### 2. Image

Copy directly from the main app service.

```yaml
image: crocodilestick/calibre-web-automated:latest
```
```nix
image = "crocodilestick/calibre-web-automated:latest";
```

### 3. Environment variables

Copy from the main app service. Skip `TZ`, `PUID`, `PGID` — added
automatically. Skip `TS_*` — handled by the sidecar.

```yaml
environment:
  - PUID=1000     # skip — default
  - PGID=1000     # skip — default
  - TZ=UTC        # skip — default
  - CUSTOM_VAR=foo
```
```nix
environment = {
  CUSTOM_VAR = "foo";
};
```

### 4. Volumes

Replace leading `./` with `${dataDir}/`. Keep the container path as-is.
Drop `:Z` SELinux labels — not needed on NixOS.

```yaml
volumes:
  - ./config:/config
  - ./data:/app/data
```
```nix
volumes = [
  "${dataDir}/config:/config"
  "${dataDir}/data:/app/data"
];
```

### 5. Internal port

If the app listens on a port other than `80`, set `servePort`:

```nix
containerLib.mkTailscaleService {
  name = "calibre";
  image = "...";
  servePort = 8083;
  volumes = [ ... ];
};
```

### 6. Tailscale sidecar

Ignore the entire tailscale block in compose — generated automatically.
Only carry over `hostname` if it differs from the service name:

```yaml
hostname: calibre   # becomes hostname = "calibre"; in mkTailscaleService
```

### 7. Extra container options

`cap_add`, `devices`, `network_mode`, and `depends_on` are all handled
automatically. Do not specify them.

---

## Example: Calibre Web Automated

Given this compose file:

```yaml
services:
  tailscale-calibre:
    image: tailscale/tailscale:latest
    hostname: calibre
    environment:
      - TS_AUTHKEY=${TS_AUTHKEY}
      - TS_STATE_DIR=/var/lib/tailscale
      - TS_SERVE_CONFIG=/config/serve.json
    volumes:
      - ./tailscale:/var/lib/tailscale:Z
      - ./serve.json:/config/serve.json:ro,Z
    cap_add:
      - NET_ADMIN
      - SYS_MODULE
    devices:
      - /dev/net/tun:/dev/net/tun

  calibre-web-automated:
    image: crocodilestick/calibre-web-automated:latest
    network_mode: service:tailscale-calibre
    depends_on:
      - tailscale-calibre
    environment:
      - PUID=1000
      - PGID=1000
      - TZ=UTC
    volumes:
      - ./config:/config
      - ./ingest:/cwa-book-ingest
      - ./calibre-library:/calibre-library
```

The output module is:

```nix
# modules/services/calibre.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.modules.calibre;
  dataDir = "/mnt/services/calibre";
  containerLib = import ../../lib/container.nix { inherit config pkgs; };
in
{
  options.modules.calibre = {
    enable = lib.mkEnableOption "Calibre Web Automated ebook manager";
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir}                  0755 root root -"
      "d ${dataDir}/tailscale        0755 root root -"
      "d ${dataDir}/config           0755 root root -"
      "d ${dataDir}/ingest           0755 root root -"
      "d ${dataDir}/calibre-library  0755 root root -"
    ];

    virtualisation.oci-containers.containers =
      containerLib.mkTailscaleService {
        name = "calibre";
        hostname = "calibre";
        image = "crocodilestick/calibre-web-automated:latest";
        servePort = 8083;
        volumes = [
          "${dataDir}/config:/config"
          "${dataDir}/ingest:/cwa-book-ingest"
          "${dataDir}/calibre-library:/calibre-library"
        ];
      };

    systemd.services."podman-tailscale-calibre" = {
      after = [ "podman-calibre.service" ];
      requires = [ "podman-calibre.service" ];
    };
  };
}
```

---

## Migrating Existing Data

Existing service data lives at `/etc/nixos/services/<name>/` on the server.
Migrate one service at a time.

### Migration Steps

**1. Inspect the existing service**

```bash
ls /etc/nixos/services/<name>/
cat /etc/nixos/services/<name>/compose.yaml
```

Note which folders exist — these are the volumes that need moving.

**2. Stop the existing containers**

```bash
cd /etc/nixos/services/<name>
podman compose down
podman ps
```

**3. Write and deploy the new module**

Create `modules/services/<name>.nix` following the template above, enable it
in `hosts/nixos-server/default.nix`, commit, then deploy from your local
machine:

```bash
just deploy-server
```

Do not use `just switch` on the server — that builds from the local server
checkout and will not pick up updated agenix secrets.

**4. Copy the data**

Use `cp -a src/. dst/` (note the trailing `/.`) to copy directory *contents*
rather than creating a nested directory:

```bash
sudo mkdir -p /mnt/services/<name>
sudo cp -a /etc/nixos/services/<name>/config/.    /mnt/services/<name>/config/
sudo cp -a /etc/nixos/services/<name>/tailscale/. /mnt/services/<name>/tailscale/
# repeat for each volume folder
```

> **Note:** `serve.json` is now generated from the Nix store — do not copy it.

**5. Start the new units**

```bash
sudo systemctl start podman-<name>
sudo systemctl start podman-tailscale-<name>
```

**6. Verify**

```bash
systemctl status podman-<name> podman-tailscale-<name>
journalctl -u podman-<name> -u podman-tailscale-<name> -f
tailscale status | grep <name>
```

Check the Tailscale domain is reachable and data is intact.

**7. Remove the old directory**

```bash
sudo rm -rf /etc/nixos/services/<name>
```

---

## Checklist Per Service

**Conversion**
- [ ] Create `modules/services/<name>.nix` from the template above
- [ ] Set `name`, `image`, `volumes` from the compose file
- [ ] Set `servePort` if the app does not listen on port `80`
- [ ] Set `hostname` if it differs from `name`
- [ ] Add a `systemd.tmpfiles.rules` entry for every volume directory
- [ ] Enable it in `hosts/nixos-server/default.nix` with `modules.<name>.enable = true;`

**Secrets**
- [ ] Confirm `ts_authkey.age` secret is in `TS_AUTHKEY=<key>` format
- [ ] Confirm the auth key is **reusable** (not one-time)

**Migration**
- [ ] Stop old containers with `podman compose down`
- [ ] Deploy config with `just deploy-server` from local machine
- [ ] Copy volume folders using `cp -a src/. dst/`
- [ ] Start new systemd units
- [ ] Verify service is running and data is intact
- [ ] Remove `/etc/nixos/services/<name>/` only after verification

---

## Day-to-Day Operations

All containers managed by `oci-containers` become systemd units named
`podman-<name>`. The Tailscale sidecar is `podman-tailscale-<name>`.

### Viewing Running Services

```bash
podman ps
systemctl list-units 'podman-*'
```

### Status and Logs

```bash
systemctl status podman-<name> podman-tailscale-<name>

journalctl -u podman-<name> -f
journalctl -u podman-<name> -n 100
journalctl -u podman-<name> -u podman-tailscale-<name> -f
```

### Starting and Stopping

```bash
# stop (sidecar first — it depends on the app)
systemctl stop podman-tailscale-<name> podman-<name>

# start (app first)
systemctl start podman-<name>
systemctl start podman-tailscale-<name>

# restart
systemctl restart podman-<name>
systemctl restart podman-tailscale-<name>
```

### Applying Config Changes

```bash
# from local machine
just deploy-server
```

After a deploy, systemd restarts any containers whose definition changed.
Unchanged containers are left running.

### Shell Access

```bash
podman ps
podman exec -it <name> /bin/sh
podman exec -it <name> /bin/bash   # linuxserver images have bash
```

Note: `machinectl` does not work with podman containers.

---

## Tailscale URL

Each service is reachable at:

```
https://<hostname>.<tailnet>.ts.net
```

The hostname is the `hostname` param passed to `mkTailscaleService` (defaults
to `name`). Find your tailnet name with `tailscale status`.

If a new container registers with a `-1` suffix (e.g. `freshrss-1`), it means
an old node with the same hostname still exists in the Tailscale admin console.
Delete the old node, wipe the tailscale state directory, and restart the
container:

```bash
sudo rm -rf /mnt/services/<name>/tailscale
sudo mkdir -p /mnt/services/<name>/tailscale
sudo systemctl restart podman-tailscale-<name>
```
