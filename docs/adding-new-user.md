# Adding a New User to This NixOS Configuration

## Overview

Everything is organized by user. Each user has their own directory containing their home-manager config and their machine(s).

```
users/
└── mom/
    ├── default.nix        # Home-manager identity config (username, stateVersion, personal imports)
    ├── common.nix         # System user config (users.users.mom)
    └── hosts/
        └── moms-pc/
            ├── default.nix    # Host config (dot-notation enables)
            └── hardware.nix
```

Modules are auto-imported — you never need to manually reference file paths. Just enable what you need.

All modules live in `modules/` and work at the NixOS level. Home-manager config is wrapped inside modules using `home-manager.users.${user} = { ... }`, so you can enable any module from any host config.

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

Create `users/mom/default.nix`. This is for identity and personal config only — module enables go in the host config:

```nix
{ ... }:

{
  home.username = "mom";
  home.homeDirectory = "/home/mom";
  home.stateVersion = "25.11";
}
```

## Step 4: Create the Host Configuration

Create `users/mom/hosts/moms-pc/default.nix`. All modules (including home-manager ones) are enabled here:

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

  # Modules — NixOS and home-manager both enabled from here
  desktops.gnome.enable = true;
  services.tailscaleDesktop.enable = true;
  services.sshServer.enable = true;
  services.flatpakBase.enable = true;
  security.yubikey.enable = true;
  shell.enable = true;
  git.enable = true;
  terminal.enable = true;

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

All modules are enabled from the host `default.nix` using dot-notation. Modules that configure home-manager do so internally — no separate user-level enable needed.

### Desktop / System Modules

| Option | Description |
|--------|-------------|
| `modules.gnome.enable` | GNOME + GDM + Pipewire |
| `modules.kde.enable` | KDE Plasma + SDDM + Pipewire |
| `modules.gaming.enable` | Steam + Gamemode + Lutris |
| `modules.development.enable` | Podman + libvirt + dev tools |
| `modules.yubikey.enable` | Polkit + Yubikey + GPG |
| `modules.autoUpgrade.enable` | Auto-pull from GitHub and rebuild |
| `modules.tailscale.enable` | Tailscale with systray |
| `modules.ssh.enable` | OpenSSH server |
| `modules.vpn.enable` | VPN support |
| `modules.flatpak.enable` | Base Flatpak setup |

### User Environment Modules

| Option | Description |
|--------|-------------|
| `modules.shell.enable` | Fish shell + Starship + CLI tools |
| `modules.git.enable` | Git + lazygit |
| `modules.terminal.enable` | Ghostty terminal |
| `modules.media.enable` | Audio/media tools |
| `modules.editors.vscode.enable` | VSCode + extensions |
| `modules.editors.neovim.enable` | Neovim |
| `modules.editors.zed.enable` | Zed editor |
| `modules.browsers.zen.enable` | Zen browser + addons |

### Server / Headless Modules

| Option | Description |
|--------|-------------|
| `modules.server.ssh.enable` | Hardened SSH (no root, no password auth) |
| `modules.server.tailscale.enable` | Tailscale headless |
| `modules.server.podman.enable` | Rootless Podman + user socket |
| `modules.server.arion.enable` | Arion (declarative Docker/Podman) |
| `modules.server.restic.enable` | Backups to Backblaze B2 |
| `modules.server.autoUpgrade.enable` | Git pull + rebuild |
| `modules.server.containerAutoUpdate.enable` | Daily container updates |
| `modules.server.services.freshrss.enable` | FreshRSS via Arion |

---

## Creating a New Module

All modules live in `modules/` and follow the same pattern. If the module configures home-manager, wrap that config in `home-manager.users.${user} = { ... }`:

```nix
# modules/my-feature.nix
{ config, lib, pkgs, user, ... }:

let cfg = config.myFeature; in
{
  options.myFeature = {
    enable = lib.mkEnableOption "description shown in LSP hover";
  };

  config = lib.mkIf cfg.enable {
    # NixOS config (optional)
    programs.fish.enable = true;

    # Home-manager config (optional)
    home-manager.users.${user} = {
      home.packages = with pkgs; [ my-tool ];
    };
  };
}
```

Enable it in any host:

```nix
myFeature.enable = true;
```

Auto-imported — no manual file path imports needed. `git add` the new file before building.

---

## Troubleshooting

**"path does not exist" errors**
New files must be git-tracked before Nix can see them:
```bash
git add -A
```

**Option `X` does not exist**
The module defining `options.X.enable` isn't being swept by import-tree. Check:
- The module is in `modules/` (or a subdirectory — any name without a leading `_`)
- The file has been `git add`-ed
