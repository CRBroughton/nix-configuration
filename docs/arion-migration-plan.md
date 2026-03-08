# Plan: Migrate Services to Arion

## Goal

Replace compose.yaml files with Nix-native arion definitions. This enables:
- Full declarative container management via `just deploy-server`
- No more rsync/git pull for service config changes
- Type-checked container definitions
- Secrets managed via environmentFiles (keeps .env pattern)

## Current State

- ~10 services using compose.yaml + tailscale sidecar pattern
- Rootless podman via home-manager (podman-flake)
- Data stored in `/etc/nixos/services/<service>/data/`
- Secrets in `.env` files per service

## Target State

- Arion modules in `modules/server/services/`
- Containers run as system services (root podman or docker)
- Data stays in `/var/lib/arion/<service>/` or existing paths
- Secrets in `/etc/nixos/services/<service>/.env` (unchanged)

## Prerequisites

1. Decide: Docker or Podman backend for arion
   - Docker: better arion compatibility, more tested
   - Podman: already familiar, rootless option
   - Recommendation: Use Docker for arion (system-level), simpler

2. Backup current service data

## Implementation Steps

### Phase 1: Setup Arion Infrastructure

1. Add arion to flake.nix:
   ```nix
   inputs.arion = {
     url = "github:hercules-ci/arion";
     inputs.nixpkgs.follows = "nixpkgs";
   };
   ```

2. Create arion module structure:
   ```
   modules/server/
   ├── arion.nix              # Base arion config
   └── services/
       ├── freshrss.nix
       ├── calibre.nix
       ├── adguard.nix
       ├── xmpp.nix
       ├── mumble.nix
       ├── thelounge.nix
       ├── ergo.nix
       ├── glance.nix
       └── ...
   ```

3. Create base arion module (`modules/server/arion.nix`):
   ```nix
   { config, pkgs, inputs, ... }:
   {
     imports = [ inputs.arion.nixosModules.arion ];

     virtualisation.docker.enable = true;  # or podman
     virtualisation.arion.backend = "docker";  # or "podman-socket"

     # Common arion settings
   }
   ```

### Phase 2: Create Tailscale Sidecar Pattern

Create reusable function for tailscale + service pattern:

```nix
# lib/arion.nix
{ pkgs }:
{
  # Creates a tailscale sidecar + main service
  mkTailscaleService = {
    name,           # e.g., "freshrss"
    hostname,       # tailscale hostname
    image,          # main service image
    environment ? {},
    volumes ? [],
    servicePort ? 80,
  }: {
    project.name = name;

    services = {
      "tailscale-${name}" = {
        service = {
          image = "tailscale/tailscale:latest";
          hostname = hostname;
          restart = "unless-stopped";
          environment = {
            TS_STATE_DIR = "/var/lib/tailscale";
            TS_SERVE_CONFIG = "/config/serve.json";
          };
          volumes = [
            "/var/lib/arion/${name}/tailscale:/var/lib/tailscale"
            "/etc/nixos/services/${name}/serve.json:/config/serve.json:ro"
          ];
          capabilities.NET_ADMIN = true;
          capabilities.SYS_MODULE = true;
          devices = [ "/dev/net/tun:/dev/net/tun" ];
          environmentFiles = [ "/etc/nixos/services/${name}/.env" ];
        };
      };

      "${name}" = {
        service = {
          inherit image;
          restart = "unless-stopped";
          network_mode = "service:tailscale-${name}";
          depends_on = [ "tailscale-${name}" ];
          environment = {
            TZ = "UTC";
          } // environment;
          volumes = volumes;
        };
      };
    };
  };
}
```

### Phase 3: Migrate Services One-by-One

Order by complexity (simplest first):

#### 3.1 FreshRSS (simple, good test case)
```nix
# modules/server/services/freshrss.nix
{ config, lib, arionLib, ... }:
{
  virtualisation.arion.projects.freshrss = arionLib.mkTailscaleService {
    name = "freshrss";
    hostname = "freshrss";
    image = "lscr.io/linuxserver/freshrss:latest";
    environment = {
      PUID = "1000";
      PGID = "1000";
    };
    volumes = [
      "/var/lib/arion/freshrss/config:/config"
    ];
  };
}
```

#### 3.2 Migration order:
1. freshrss - simple, single container + tailscale
2. calibre - similar pattern
3. glance - simple dashboard
4. adguard - DNS, test carefully
5. thelounge - IRC client
6. ergo - IRC server (custom config)
7. mumble - voice chat
8. xmpp - most complex (certs, multiple ports)

### Phase 4: Data Migration

For each service:
```bash
# 1. Stop old service
cd /etc/nixos/services/freshrss && podman-compose down

# 2. Move data to new location (or keep in place)
sudo mkdir -p /var/lib/arion/freshrss
sudo mv /etc/nixos/services/freshrss/volume /var/lib/arion/freshrss/config

# 3. Deploy arion service
just deploy-server

# 4. Verify service works
curl https://freshrss.tail538465.ts.net
```

### Phase 5: Secrets Management

Keep using .env files with `environmentFiles`:
```nix
service.environmentFiles = [ "/etc/nixos/services/${name}/.env" ];
```

The .env files stay on the server (not in git). Arion reads them at container start.

Future improvement: Consider sops-nix or agenix for encrypted secrets in git.

### Phase 6: Cleanup

After all services migrated and verified:

1. Remove old compose.yaml files from git
2. Remove podman-flake from server home-manager
3. Remove `services/` directory compose files
4. Update justfile (remove compose commands, add arion commands)
5. Update README

## Updated Justfile Commands

```just
# Arion service management
arion-up service:
    sudo arion --prebuilt-file /etc/arion/{{service}}/arion-compose.json up -d

arion-down service:
    sudo arion --prebuilt-file /etc/arion/{{service}}/arion-compose.json down

arion-logs service:
    sudo arion --prebuilt-file /etc/arion/{{service}}/arion-compose.json logs -f
```

Or just use systemctl since arion creates systemd services:
```just
service-restart service:
    sudo systemctl restart arion-{{service}}

service-logs service:
    sudo journalctl -fu arion-{{service}}
```

## Service-Specific Notes

### AdGuard Home
- DNS on port 53 requires root/capabilities
- Test DNS resolution carefully before/after

### XMPP (Prosody)
- Tailscale certs need to be accessible
- Multiple ports (5222, 5269, 5280)
- May need custom serve.json handling

### Mumble
- UDP ports for voice
- Test with actual client

## Rollback Plan

If migration fails for a service:
1. Keep compose.yaml files until fully verified
2. Can always `podman-compose up` the old way
3. Data stays in place, just change which system manages containers

## Success Criteria

- [ ] All services accessible via Tailscale hostnames
- [ ] `just deploy-server` deploys everything (no SSH + git pull needed)
- [ ] Service data persisted correctly
- [ ] Secrets loaded from .env files
- [ ] Old podman-flake removed from server config
- [ ] README updated with new workflow

## Estimated Time

- Phase 1 (setup): 1 hour
- Phase 2 (sidecar pattern): 1-2 hours
- Phase 3 (migrate services): 3-4 hours (30 min each)
- Phase 4 (data migration): 1 hour
- Phase 5-6 (cleanup): 1 hour

**Total: 7-9 hours** (can be done across multiple sessions)

## References

- [Arion documentation](https://docs.hercules-ci.com/arion/)
- [Arion examples](https://github.com/hercules-ci/arion/tree/master/examples)
- [NixOS containers wiki](https://nixos.wiki/wiki/NixOS_Containers)
