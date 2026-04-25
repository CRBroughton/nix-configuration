# NixOS Configuration

NixOS configuration for my machines: laptop, gaming PC, and more.

## Want to use this as a starting point?

A minimal template based on this configuration is available in the [`template/`](template/) folder. It includes the same user/host structure and `mkHost` helper, but with no desktop environment, no custom shell, and no opinionated defaults — just the bare minimum to build from.

See [`template/README.md`](template/README.md) for setup instructions and guidance on expanding it.

## Structure

```
nix-configuration/
├── flake.nix                      # Entry point - defines inputs and hosts
├── lib/
│   ├── default.nix                # Helper functions (mkHost, mkPi)
│   └── arion.nix                  # Arion helpers (mkTailscaleService)
├── hosts/                         # One directory per machine
│   ├── laptop/
│   │   ├── default.nix            # NixOS config (modules, packages, services)
│   │   └── hardware.nix
│   ├── gaming-pc/
│   │   ├── default.nix
│   │   └── hardware.nix
│   ├── brighton-pc/
│   │   ├── default.nix
│   │   └── hardware.nix
│   ├── nixos-server/              # Home server
│   │   ├── default.nix
│   │   ├── hardware.nix
│   │   └── users/craig/home.nix   # Per-host home-manager override
│   ├── pi-monitor/                # Raspberry Pi monitoring
│   │   └── default.nix
│   ├── moons-pc/
│   │   ├── default.nix
│   │   └── hardware.nix
│   ├── mum-pc/
│   │   ├── default.nix
│   │   └── hardware.nix
│   └── mums-laptop/
│       ├── default.nix
│       └── hardware.nix
├── users/                         # User configs (HM + system user definition)
│   └── craig/
│       ├── default.nix            # Home-manager config (shell, editors, etc.)
│       ├── git.nix                # Personal git config (name, email, keys)
│       ├── gnome.nix              # Personal GNOME settings (theme, dock, extensions)
│       ├── flatpaks.nix           # Personal Flatpak apps
│       ├── common.nix             # System user config (users.users.craig) — auto-imported by mkHost
│       └── vm-testing.nix         # VM testing settings
├── modules/                       # Domain-based modules
│   ├── common.nix                 # Packages for ALL systems (git, nixfmt, etc.)
│   ├── gaming.nix                 # Steam + gamemode + Lutris (combined)
│   ├── security.nix               # Polkit + Yubikey (combined)
│   ├── development.nix            # Podman + libvirt + dev tools (combined)
│   ├── tailscale.nix              # Tailscale service + systray (combined)
│   ├── nix.nix                    # Nix settings, garbage collection
│   ├── auto-upgrade.nix           # Auto-pull from GitHub and rebuild daily
│   ├── shell.nix                  # Fish, Starship, Zoxide, CLI tools
│   ├── terminal.nix               # Ghostty terminal
│   ├── git.nix                    # Git config (generic)
│   ├── media.nix                  # Audio/communication apps
│   ├── zen-browser.nix            # Zen Browser + addons
│   ├── desktop/
│   │   ├── gnome.nix              # GDM + GNOME + Pipewire (base system)
│   │   └── kde.nix                # SDDM + KDE Plasma + Pipewire (base system)
│   ├── services/
│   │   ├── ssh.nix
│   │   ├── vpn.nix
│   │   └── flatpak/base.nix       # Base Flatpak apps
│   ├── editors/
│   │   ├── vscode.nix
│   │   ├── neovim.nix
│   │   └── zed.nix
│   └── server/                    # Server-specific modules
│       ├── podman.nix             # Rootless Podman for servers
│       ├── tailscale.nix          # Tailscale (headless, no systray)
│       ├── ssh.nix                # Hardened SSH config
│       ├── restic.nix             # Backups to Backblaze B2
│       ├── auto-upgrade.nix       # Git pull + rebuild for servers
│       ├── container-auto-update.nix  # Daily podman-compose updates
│       ├── arion.nix              # Docker + Arion base config
│       └── services/              # Arion service definitions
│           ├── freshrss.nix       # FreshRSS + Tailscale sidecar
│           └── harmonia.nix       # Nix binary cache server
├── services/                      # Podman compose services (for server)
│   ├── adguard/                   # DNS ad blocker
│   ├── calibre/                   # Ebook manager
│   ├── copyparty/                 # File sharing
│   ├── freshrss/                  # RSS reader
│   ├── glance/                    # Dashboard
│   ├── irc/                       # Ergo IRC server
│   ├── linkding/                  # Bookmark manager
│   ├── mealie/                    # Recipe manager
│   ├── mumble/                    # Voice server
│   ├── nostr/                     # Nostr relay
│   ├── openwebui/                 # LLM chat interface (Ollama)
│   ├── restic-exporter/           # Backup stats exporter
│   ├── searxng/                   # Privacy search engine
│   ├── thelounge/                 # Web IRC client
│   └── xmpp/                      # Prosody XMPP server
├── disko/                         # Declarative disk partitioning
│   ├── laptop.nix
│   └── gaming-pc.nix
├── docs/                          # Documentation
│   └── adding-new-user.md         # Guide for adding new users/machines
├── config/                        # External config files
│   └── neovim/                    # Neovim configuration
├── xkb/                           # Custom keyboard layouts
└── fonts/                         # Custom fonts
```

**Combined modules** set both NixOS and home-manager options in one file. This keeps related config together by domain rather than splitting by system level.

## Current Hosts

### laptop (Dell XPS)

| Category | Details |
|----------|---------|
| Desktop | GNOME (Wayland) |
| Kernel | Zen |
| Shell | Fish + Starship |
| Editors | VS Code, Neovim, Zed |
| Browser | Zen Browser |
| Terminal | Ghostty |
| Gaming | Steam, Gamemode, Gamescope, Lutris |
| Development | Podman, libvirt, virt-manager, Go, Zig, Node.js, Rust, PHP |
| Services | SSH, Tailscale, VPN |
| Security | Polkit, Yubikey support, GPG tools |
| Extras | Power profiles daemon, fwupd, auto-upgrade |

### gaming-pc

| Category | Details |
|----------|---------|
| Desktop | GNOME (Wayland) |
| Kernel | Zen (CachyOS + scx_lavd available) |
| Shell | Fish + Starship |
| Editors | VS Code, Neovim, Zed |
| Browser | Zen Browser |
| Terminal | Ghostty |
| Gaming | Steam, Gamemode, Gamescope, Lutris |
| Development | Podman, libvirt, virt-manager, Go, Zig, Node.js, Rust, PHP |
| Services | SSH, Tailscale, VPN |
| Security | Polkit, Yubikey support, GPG tools |
| Extras | fwupd |

### nixos-server (Home Server)

| Category | Details |
|----------|---------|
| Type | Headless server (no desktop) |
| Shell | Fish + Starship |
| Containers | Rootless Podman with compose |
| Services | AdGuard, Calibre, FreshRSS, Mealie, Mumble, IRC (Ergo), XMPP (Prosody), Open WebUI + Ollama, SearXNG, Linkding, Glance dashboard |
| Networking | Tailscale (all services exposed via Tailscale Funnel) |
| Binary Cache | Harmonia (serves local Nix store to all machines via Tailscale) |
| Backups | Restic to Backblaze B2 (daily at 02:00) |
| Auto-update | Git pull + rebuild (daily at 04:00), container updates (daily at 05:00) |

### pi-monitor (Raspberry Pi 3 B+)

| Category | Details |
|----------|---------|
| Type | Monitoring node |
| Services | Uptime Kuma (monitors other services) |
| Networking | Tailscale |
| Build | SD card image (`nix build .#images.pi-monitor`) |

Both desktop hosts (laptop, gaming-pc) share the same user config (craig) with identical home-manager modules. The server uses a minimal home-manager config without desktop apps.

## Quick Reference

| Task | Command |
|------|---------|
| Apply changes | `just switch` |
| Update flake inputs | `just update` |
| Update and apply | `just update-all` |
| Test in VM | `just vm` |
| Clean old generations | `just clean` |
| Check for errors | `just check` |
| See all commands | `just` |

## Usage Guide

### Applying Configuration Changes

After editing any `.nix` file:

```bash
just switch
```

Or with verbose output for debugging:

```bash
just switch-verbose
```

### Updating the System

Update all flake inputs (nixpkgs, home-manager, etc.):

```bash
just update        # Update flake.lock
just switch        # Apply updates
```

Or in one command:

```bash
just update-all
```

### Adding Packages

**Global packages** (all systems - laptop, server, Pi):
Add to `modules/common.nix`:
```nix
environment.systemPackages = with pkgs; [
  git
  nixfmt
  your-package  # Add here
];
```

**System packages** (specific host):
Add to `environment.systemPackages` in host config or a module.

**User packages** (home-manager):
Edit the appropriate module in `modules/`:

| Package Type | File |
|-------------|------|
| CLI tools | `modules/shell.nix` |
| Dev languages/tools | `modules/development.nix` (combined module) |
| Gaming | `modules/gaming.nix` (combined module) |
| Media/audio apps | `modules/media.nix` |
| Security/Yubikey | `modules/security.nix` (combined module) |

Example - adding `htop`:
```nix
# modules/shell.nix
home.packages = with pkgs; [
  bat
  eza
  htop  # Add here
  ...
];
```

Then run `just switch`.

**Flatpak apps**:

Base flatpaks (everyone gets): `modules/services/flatpak/base.nix`
Personal flatpaks (imported by host): `users/craig/flatpaks.nix`

```nix
services.flatpak.packages = [
  "com.spotify.Client"  # Add Flatpak app ID
  ...
];
```

### Adding VSCode Extensions

Edit `modules/editors/vscode.nix`:

```nix
extensions = with pkgs.vscode-marketplace; [
  publisher.extension-name  # Add extension
  ...
];
```

Find extension IDs on the VS Code marketplace (format: `publisher.extension-name`).

### Changing GNOME Settings

Edit your personal GNOME config (e.g., `users/craig/gnome.nix`):

```nix
dconf.settings = {
  "org/gnome/desktop/interface" = {
    color-scheme = "prefer-dark";
    accent-color = "purple";  # Change accent color
  };
  # Add more dconf paths here
};
```

To find dconf paths, use `dconf watch /` and change settings in GNOME Settings.

### Adding GNOME Extensions

In your personal GNOME config (e.g., `users/craig/gnome.nix`):

1. Add the extension package:
   ```nix
   home.packages = with pkgs; [
     gnomeExtensions.extension-name
   ];
   ```

2. Add to enabled extensions:
   ```nix
   enabled-extensions = [
     "extension-id@author"
   ];
   ```

### Modifying the Dock (Favorite Apps)

Edit your personal GNOME config (e.g., `users/craig/gnome.nix`):

```nix
favorite-apps = [
  "app.zen_browser.zen.desktop"
  "org.gnome.Nautilus.desktop"
  # Add/remove/reorder apps here
];
```

Find `.desktop` file names in `/run/current-system/sw/share/applications/` or `~/.local/share/flatpak/exports/share/applications/`.

### Enabling Auto-Upgrade

For machines that should automatically stay up-to-date (great for family members), add the auto-upgrade module:

```nix
(modules + "/auto-upgrade.nix")
```

This will:
- Pull the latest config from GitHub daily
- Rebuild the system automatically
- Run as a systemd timer (check status: `systemctl status nixos-upgrade`)

Edit `modules/auto-upgrade.nix` to customize:
- `dates` - when to check ("daily", "weekly", or cron syntax like "04:00")
- `allowReboot` - auto-reboot after kernel updates
- `rebootWindow` - only reboot during specific hours

### Creating a New Module

1. Create the file in `modules/<name>.nix`

2. Choose the appropriate structure:

   **Home-only module** (e.g., shell, editors):
   ```nix
   { config, pkgs, ... }:
   {
     home.packages = with pkgs; [ ... ];
     programs.something.enable = true;
   }
   ```

   **Combined module** (both system and user config):
   ```nix
   { config, pkgs, user, ... }:
   {
     # NixOS options
     services.something.enable = true;

     # Home-manager options
     home-manager.users.${user} = {
       home.packages = with pkgs; [ ... ];
     };
   }
   ```

3. Import it in a host config (`hosts/<hostname>/default.nix`) using the `modules` path shortcut:
   ```nix
   imports = [
     (modules + "/<name>.nix")
   ];
   ```

### Adding a New Machine

See [docs/adding-new-user.md](docs/adding-new-user.md) for the complete guide.

**Quick summary:**

1. Create the host and (if new) user directories:
   ```bash
   mkdir -p hosts/<hostname>
   mkdir -p users/<username>   # skip if user already exists
   ```

2. Create the required files:
   - `hosts/<hostname>/default.nix` - NixOS host config
   - `hosts/<hostname>/hardware.nix` - Hardware config
   - `users/<username>/default.nix` - Home-manager config (if new user)
   - `users/<username>/common.nix` - System user config (if new user) — auto-imported by `mkHost`

3. Add to `flake.nix`:
   ```nix
   <hostname> = myLib.mkHost {
     hostname = "<hostname>";
     users = [ "<username>" ];   # list — supports multiple users per host
     inherit stateVersion;
     extraModules = [ modules ];
   };
   ```

### Testing Changes Safely

Test in a VM before deploying to real hardware:

```bash
just vm          # Build and run laptop VM
just vm-gaming   # Build and run gaming-pc VM
```

VM login: user `craig`, password `test` (set via `vm-testing.nix`).

### Migrating from nix-home-server

If you're migrating from the old `nix-home-server` repo, use the migration script:

```bash
# On the server:
curl -O https://raw.githubusercontent.com/CRBroughton/nix-configuration/nixos-migration/scripts/migrate-server.sh
chmod +x migrate-server.sh
sudo ./migrate-server.sh
```

Or manually copy and run `scripts/migrate-server.sh`. The script will:
1. Stop all containers
2. Backup your data
3. Clone this repo
4. Copy your service data (volumes, configs, .env files)
5. Switch to the new configuration
6. Restart all containers

### Managing the Home Server

**Deploy to server (from another machine):**
```bash
nixos-rebuild switch --flake .#nixos-server --target-host craig@nixos-server
```

**Starting/stopping services:**
```bash
# SSH into server, then:
cd /etc/nixos/services/<service-name>
podman-compose up -d    # Start
podman-compose down     # Stop
podman-compose logs -f  # View logs
```

**Managing containers:**
```bash
lazydocker              # Interactive container management
podman ps               # List running containers
```

**Check backup status:**
```bash
sudo systemctl status restic-backups-b2
sudo journalctl -u restic-backups-b2 -f  # View logs
```

### Setting Up the Binary Cache (First Time)

The server runs [Harmonia](https://github.com/nix-community/harmonia) as a Nix binary cache, served to all machines over Tailscale. This means custom packages (chaotic kernel, vscode extensions overlay, etc.) are built once on the server and shared — other machines download instead of rebuilding.

**Step 1 — Generate the signing key on nixos-server (one-time, manual):**

```bash
sudo mkdir -p /etc/harmonia
sudo nix-store --generate-binary-cache-key nixos-server \
  /etc/harmonia/signing-key.secret \
  /etc/harmonia/signing-key.pub
cat /etc/harmonia/signing-key.pub   # copy this
tailscale ip -4                     # copy the server's Tailscale IP
```

**Step 2 — Fill in the values in `modules/nix.nix`:**

Replace `TAILSCALE_IP` and `HARMONIA_PUBLIC_KEY` with the values from Step 1.

**Step 3 — Deploy the server first, then other machines:**

```bash
# On the server
just switch

# Verify harmonia is running
systemctl status harmonia
curl http://localhost:5000/nix-cache-info

# Then rebuild other machines (they will use the cache)
nixos-rebuild switch --flake .#laptop
```

The cache starts empty and fills naturally as machines build packages. The first rebuild is no different to normal — subsequent rebuilds skip anything the server already has.

### Building the Pi SD Card Image

```bash
nix build .#images.pi-monitor
# Image will be in result/sd-image/*.img.zst
# Flash to SD card with:
zstd -d result/sd-image/*.img.zst -o pi.img
sudo dd if=pi.img of=/dev/sdX bs=4M status=progress
```

**Deploy updates to running Pi:**
```bash
nixos-rebuild switch --flake .#pi-monitor --target-host craig@pi-monitor
```

### Rollback

If something breaks after switching:

```bash
just rollback    # Switch to previous generation
```

Or select a previous generation from the boot menu.

### Maintenance

```bash
just clean       # Remove generations older than 30 days
just clean-all   # Remove ALL old generations
just optimise    # Deduplicate nix store
just maintenance # Clean + optimise
```

## Installation

### Fresh NixOS Install

1. Boot NixOS installer USB (minimal ISO should work, you dont need to actually use the ISO steps)
2. Connect to network
3. Clone this repo:
   ```bash
   nix-shell -p git just micro
   git clone https://github.com/CRBroughton/nix-configuration.git
   cd nix-configuration
   ```
4. Partition disk:
   ```bash
   just disko-laptop device="/dev/nvme0n1"
   ```
5. Generate hardware config:
   ```bash
   nixos-generate-config --root /mnt --show-hardware-config > hosts/laptop/hardware.nix
   ```
6. Install:
   ```bash
   just install-laptop
   ```
7. Reboot, then set your password:
   ```bash
   passwd
   ```
8. Copy your SSH private key to `~/.ssh/id_ed25519`

### Existing NixOS System

```bash
git clone https://github.com/CRBroughton/nix-configuration.git
cd nix-configuration
sudo nixos-rebuild switch --flake .#laptop
```

## Troubleshooting

### Build fails with "file not found"

Make sure all new files are tracked by git:
```bash
git add -A
```

### Flatpak apps not appearing

Flatpaks install on first boot/switch. Check status:
```bash
flatpak list
```

### VS Code extensions not loading

Extensions are managed by Nix. Edit `modules/editors/vscode.nix` and run `just switch`.

### Finding package names

```bash
nix search nixpkgs <name>
```

Or browse https://search.nixos.org/packages

### Server: Podman socket not available after reboot

If `lazydocker` shows "exit status 1" or containers can't connect to the socket:
```bash
systemctl --user enable --now podman.socket
```

### Server: XMPP/Prosody certificate errors

If Prosody logs show "Failed to load private key" or certificate errors:
```bash
# Check permissions on cert files
ls -la /etc/nixos/services/xmpp/certs/

# Fix permissions if needed
chmod 644 /etc/nixos/services/xmpp/certs/*.key
sudo chown craig:users /etc/nixos/services/xmpp/certs/*

# Or regenerate certs via Tailscale
just xmpp-gencert
podman restart prosody
```

### Server: Service missing data after migration

Some services store data in non-standard directories. Check the compose.yaml for volume mounts and ensure all data directories were copied from the old config.

### Server: Container permission denied on data files

Rootless podman remaps UIDs - container processes see different UIDs than the host. If a container can't write to its data directory, use `podman unshare` to fix ownership using the container's UIDs directly:

```bash
# 1. Find the container's UID/GID
podman exec <container> id
# Example output: uid=100(prosody) gid=102(prosody)

# 2. Fix ownership using podman unshare (translates UIDs automatically)
cd /etc/nixos/services/<service>
podman unshare chown -R <uid>:<gid> data/

# 3. Restart
podman-compose down && podman-compose up -d
```

**Examples:**

```bash
# Prosody (uid=100, gid=102)
cd /etc/nixos/services/xmpp
podman unshare chown -R 100:102 data/

# Mumble (uid=10000, gid=10000)
cd /etc/nixos/services/mumble
podman unshare chown -R 10000:10000 data/
```

`podman unshare` runs the command in podman's user namespace, so you use container UIDs and it automatically translates to the correct host UIDs.

Or use the justfile shortcut:
```bash
just fix-perms xmpp 100 102
just fix-perms mumble 10000 10000
```
