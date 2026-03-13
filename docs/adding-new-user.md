# Adding a New User to This NixOS Configuration

## Overview

Everything is organized by user. Each user has their own directory containing their home-manager config and their machine(s).

```
users/
└── mom/
    ├── default.nix        # Home-manager config (dot-notation enables)
    ├── common.nix         # System user config (users.users.mom)
    └── hosts/
        └── moms-pc/
            ├── default.nix    # Host config (dot-notation enables)
            └── hardware.nix
```

Modules are auto-imported — you never need to manually reference file paths. Just enable what you need.

---

## Step 1: Create the Directory Structure

```bash
mkdir -p users/mom/hosts/moms-pc
```

## Step 2: Create the System User Configuration

Create `users/mom/common.nix`:

```nix
{ pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.mom = {
    isNormalUser = true;
    description = "Mom";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    shell = pkgs.fish;
  };
}
```

## Step 3: Create the Home-Manager Configuration

Create `users/mom/default.nix`. Modules from `modules/_home/` are auto-imported via `sharedModules` — just enable what you need:

```nix
{ ... }:

{
  home.username = "mom";
  home.homeDirectory = "/home/mom";
  home.stateVersion = "25.11";

  # Home modules (defined in modules/_home/)
  shell.enable = true;
  git.enable = true;
  terminal.enable = true;
}
```

## Step 4: Create the Host Configuration

Create `users/mom/hosts/moms-pc/default.nix`. Modules from `modules/` are auto-imported — just enable what you need:

```nix
{ pkgs, ... }:

{
  imports = [
    ../../common.nix
    ./hardware.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # NixOS modules (defined in modules/)
  desktops.gnome.enable = true;
  services.tailscaleDesktop.enable = true;
  services.sshServer.enable = true;
  services.flatpakBase.enable = true;
  security.yubikey.enable = true;

  system.stateVersion = "25.11";
}
```

## Step 5: Create the Hardware Configuration

On the target machine:

```bash
nixos-generate-config --show-hardware-config > users/mom/hosts/moms-pc/hardware.nix
```

For VM testing first, use a minimal placeholder:

```nix
{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
  boot.kernelModules = [ "kvm-intel" "kvm-amd" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
```

## Step 6: Add to flake.nix

```nix
nixosConfigurations = {
  laptop = myLib.mkHost {
    hostname = "laptop";
    user = "craig";
    extraModules = [ modules ];
  };

  moms-pc = myLib.mkHost {
    hostname = "moms-pc";
    user = "mom";
    extraModules = [ modules ];
  };
};
```

## Step 7: Track Files and Build

```bash
git add users/mom/

# Verify
nix flake check

# Test in a VM
nixos-rebuild build-vm --flake .#moms-pc
./result/bin/run-moms-pc-vm
```

---

## Available Modules

### NixOS Modules (set in host `default.nix`)

| Option | Description |
|--------|-------------|
| `desktops.gnome.enable` | GNOME + GDM + Pipewire |
| `desktops.kde.enable` | KDE Plasma + SDDM + Pipewire |
| `gaming.enable` | Steam + Gamemode + Lutris |
| `development.enable` | Podman + libvirt + dev tools |
| `security.yubikey.enable` | Polkit + Yubikey + GPG |
| `autoUpgrade.enable` | Auto-pull from GitHub and rebuild |
| `services.tailscaleDesktop.enable` | Tailscale with systray |
| `services.sshServer.enable` | OpenSSH server |
| `services.vpn.enable` | VPN support |
| `services.flatpakBase.enable` | Base Flatpak setup |

### Home-Manager Modules (set in user `default.nix`)

| Option | Description |
|--------|-------------|
| `shell.enable` | Fish shell + Starship + CLI tools |
| `git.enable` | Git + lazygit |
| `terminal.enable` | Ghostty terminal |
| `media.enable` | Audio/media tools |
| `editors.vscode.enable` | VSCode + extensions |
| `editors.neovim.enable` | Neovim |
| `editors.zed.enable` | Zed editor |
| `browsers.zen.enable` | Zen browser + addons |

### Server Modules (set in server host `default.nix`, use `mkServer`)

| Option | Description |
|--------|-------------|
| `server.ssh.enable` | Hardened SSH (no root, no password auth) |
| `server.tailscale.enable` | Tailscale headless |
| `server.podman.enable` | Rootless Podman |
| `server.arion.enable` | Arion (declarative Docker/Podman) |
| `server.restic.enable` | Backups to Backblaze B2 |
| `server.autoUpgrade.enable` | Git pull + rebuild |
| `server.containerAutoUpdate.enable` | Daily container updates |
| `server.services.freshrss.enable` | FreshRSS via Arion |

---

## Creating a New Module

### NixOS module (in `modules/`)

```nix
# modules/my-feature.nix
{ config, lib, pkgs, ... }:

let cfg = config.myFeature; in
{
  options.myFeature = {
    enable = lib.mkEnableOption "description shown in LSP hover";
  };

  config = lib.mkIf cfg.enable {
    # your NixOS config here
  };
}
```

Enable it in any host:

```nix
myFeature.enable = true;
```

### Home-manager module (in `modules/_home/`)

```nix
# modules/_home/my-tool.nix
{ config, lib, pkgs, ... }:

let cfg = config.myTool; in
{
  options.myTool = {
    enable = lib.mkEnableOption "description";
  };

  config = lib.mkIf cfg.enable {
    # your home-manager config here
  };
}
```

Enable it in any user's `default.nix`:

```nix
myTool.enable = true;
```

Both are auto-imported — no manual file path imports needed. `git add` the new file before building.

---

## Troubleshooting

**"path does not exist" errors**
New files must be git-tracked before Nix can see them:
```bash
git add -A
```

**Option `X` does not exist**
The module defining `options.X.enable` isn't being swept by import-tree. Check:
- NixOS modules are in `modules/` (not in `_home/` or `_server/`)
- Home-manager modules are in `modules/_home/`
- Server modules are in `modules/_server/`
- The file has been `git add`-ed

**Server modules failing on desktop hosts**
Server modules live in `modules/_server/` and are only loaded by `mkServer`. Don't move them into `modules/`.
