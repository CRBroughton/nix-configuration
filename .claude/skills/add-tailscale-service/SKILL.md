---
name: add-tailscale-service
description: Add a new Docker service with a Tailscale sidecar to the services folder, and register it in the Glance dashboard config.
---

You are helping add a new Tailscale-networked Docker service. Follow these steps precisely.

## Gather information

Ask the user for (if not already provided):
1. **service name** – the short slug used for the folder and container names (e.g. `mealie`, `linkding`)
2. **docker image** – the full image reference (e.g. `ghcr.io/mealie-recipes/mealie:latest`)
3. **container port** – the port the app listens on inside the container (e.g. `9000`, `8080`)
4. **HTTP service?** – does it expose a web UI over HTTP/HTTPS? (yes → create `serve.json` and use Tailscale Funnel; no → MagicDNS only)
5. **display name** – human-readable name for Glance (e.g. `Mealie`, `The Lounge`)
6. **description** – short description for Glance (e.g. `Meal planner (web client)`)
7. **extra environment variables or volumes** – any app-specific config the user wants included

---

## Files to create

### `services/<name>/compose.yaml`

Standard pattern with Tailscale sidecar. The app container joins the sidecar's network namespace.

**For HTTP services** (with `serve.json`):
```yaml
# <Display Name> - <brief description>
# Run with: podman compose up -d

services:
  tailscale-<name>:
    image: tailscale/tailscale:latest
    container_name: tailscale-<name>
    hostname: <name>
    restart: unless-stopped
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

  <name>:
    image: <docker-image>
    container_name: <name>
    restart: unless-stopped
    network_mode: service:tailscale-<name>
    depends_on:
      - tailscale-<name>
    environment:
      TZ: Europe/London
      # add app-specific env vars here
    volumes:
      - <name>-data:/app/data   # adjust mount path to suit the image

volumes:
  <name>-data:
```

**For non-HTTP services** (no Tailscale Funnel, MagicDNS only — omit `TS_SERVE_CONFIG` and the `serve.json` volume):
```yaml
# <Display Name> - <brief description>
# Run with: podman compose up -d

services:
  tailscale-<name>:
    image: tailscale/tailscale:latest
    container_name: tailscale-<name>
    hostname: <name>
    restart: unless-stopped
    environment:
      - TS_AUTHKEY=${TS_AUTHKEY}
      - TS_STATE_DIR=/var/lib/tailscale
    volumes:
      - ./tailscale:/var/lib/tailscale:Z
    cap_add:
      - NET_ADMIN
      - SYS_MODULE
    devices:
      - /dev/net/tun:/dev/net/tun

  <name>:
    image: <docker-image>
    container_name: <name>
    restart: unless-stopped
    network_mode: service:tailscale-<name>
    depends_on:
      - tailscale-<name>
    environment:
      TZ: Europe/London
      # add app-specific env vars here
    volumes:
      - <name>-data:/app/data   # adjust mount path to suit the image

volumes:
  <name>-data:
```

---

### `services/<name>/.env`

Always create this file. Include `TS_AUTHKEY` plus any app-specific secrets gathered from the user:

```
TS_AUTHKEY=
# add any app-specific secret env vars below
```

Also create `services/<name>/.env.example` as a committed reference (values blank or annotated):

```
TS_AUTHKEY=         # Tailscale auth key from https://login.tailscale.com/admin/settings/keys
# add any app-specific secret env vars below
```

Both `.env` and `.env.example` live in the service directory. `.env` is gitignored (`services/**/.env`); `.env.example` is committed.

---

### `services/<name>/serve.json` (HTTP services only)

Replace `<port>` with the container's listening port:

```json
{
  "TCP": {
    "443": {
      "HTTPS": true
    }
  },
  "Web": {
    "${TS_CERT_DOMAIN}:443": {
      "Handlers": {
        "/": {
          "Proxy": "http://127.0.0.1:<port>"
        }
      }
    }
  }
}
```

---

## Update Glance config

Edit `services/glance/config/glance.yml` in two places:

### 1. Hide the Tailscale sidecar (in the `docker-containers` widget hidden block)

Find the block of `tailscale-*: hide: true` entries and add:
```yaml
              tailscale-<name>:
                hide: true
```

### 2. Register the app container

**For HTTP services** — add alongside other HTTP services with a `url`:
```yaml
              <container-name>:
                name: <Display Name>
                description: <description>
                url: https://<name>.tail538465.ts.net
```

**For non-HTTP / admin-only services** — add to the non-HTTP section (no `url`, or use `http://`):
```yaml
              <container-name>:
                name: <Display Name>
                description: <description>
```

The Tailscale network domain is `tail538465.ts.net`.

---

## Notes

- The `tailscale/` directory (state volume) is gitignored — do not commit it
- `.env` is gitignored (`services/**/.env`) — always create it but never commit it; commit `.env.example` instead
- `TS_AUTHKEY` must be set in `.env` (or shell env) before running `podman compose up`
- The app container must **not** publish any ports directly — all traffic goes through the Tailscale sidecar's network namespace
- If the service needs to reach other containers on the host (e.g. Ollama), add `extra_hosts: ["host.containers.internal:host-gateway"]` to the app service
- Named volumes are preferred over bind mounts for persistent data; adjust paths to match the image's documented data directory