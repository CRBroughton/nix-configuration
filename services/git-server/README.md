# Git Server

Self-hosted git server with cgit web UI, accessed over Tailscale.

- **Web UI:** https://git-server.tail538465.ts.net
- **SSH:** `ssh://git@git-server.tail538465.ts.net`

## First-time setup

```bash
mkdir -p keys repos
echo "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk" > keys/craig.pub
podman compose up -d
```

## Creating a repository

```bash
podman exec -it git-server sh -c "cd /git-server/repos && mkdir <name>.git && cd <name>.git && git init --bare"
sudo chown -R craig repos/
```

## Cloning / pushing

```bash
# Clone
git clone ssh://git@git-server.tail538465.ts.net/git-server/repos/<name>.git

# Push existing repo
git remote add origin ssh://git@git-server.tail538465.ts.net/git-server/repos/<name>.git
git push -u origin main
```

## SSH keys

Keys live in `./keys/`. Each file should contain one public key. Restart after adding:

```bash
podman compose restart git-server
```
