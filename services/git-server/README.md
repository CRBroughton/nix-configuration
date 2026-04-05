# Git Server (Soft Serve)

Self-hosted git server using [Soft Serve](https://github.com/charmbracelet/soft-serve), accessed over Tailscale.

- **Web UI:** https://git-server.tail538465.ts.net
- **SSH:** `ssh://git-server.tail538465.ts.net` (port 22)

## First-time setup

Copy `.env.example` to `.env`, then:

```bash
podman compose up -d
```

The admin key in `.env` grants immediate admin access over SSH.

## Managing repos via SSH TUI

```bash
ssh git-server.tail538465.ts.net
```

## Cloning / pushing

```bash
# Clone
git clone ssh://git-server.tail538465.ts.net/repo-name

# Push existing repo
git remote add origin ssh://git-server.tail538465.ts.net/repo-name
git push -u origin main
```

## SSH keys

The initial admin key is set via `SOFT_SERVE_ADMIN_KEY` in `.env`. Additional keys and users can be managed through the SSH TUI or web UI after first boot.
