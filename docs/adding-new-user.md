# Adding a New User to This NixOS Configuration

This guide explains how to add a new user (e.g., "mom") with their own machine to this NixOS configuration system.

## Overview

In this configuration, **everything is organized by user**. Each user has their own directory containing:
- Home-manager configuration
- System user configuration
- Their machine(s)

```
users/
└── mom/
    ├── default.nix      # Home-manager config
    ├── common.nix       # System user config (users.users.mom)
    └── hosts/
        └── moms-pc/
            ├── default.nix
            └── hardware.nix
```

## Step 1: Create the User Directory Structure

```bash
mkdir -p users/mom/hosts/moms-pc
```

## Step 2: Create the Home-Manager Configuration

Create `users/mom/default.nix`:

```nix
{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    # Only import home-only modules here
    # Combined modules (gnome, gaming, etc.) are imported in the host config
    ../../modules/shell.nix
    ../../modules/terminal.nix
    ../../modules/media.nix
    ../../modules/zen-browser.nix
  ];

  home.username = "mom";
  home.homeDirectory = "/home/mom";
  home.stateVersion = "24.11";

  # Additional packages for this user
  home.packages = with pkgs; [
    libreoffice
    vlc
  ];
}
```

## Step 3: Create the System User Configuration

Create `users/mom/common.nix`:

```nix
{ config, pkgs, ... }:

{
  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # System user
  users.users.mom = {
    isNormalUser = true;
    description = "Mom";
    extraGroups = [ "networkmanager" "audio" "video" ];
    shell = pkgs.bash;
  };
}
```

## Step 4: Create the Host Configuration

Create `users/mom/hosts/moms-pc/default.nix`:

```nix
{ config, pkgs, modules, disko, ... }:

{
  imports = [
    ../../common.nix                         # users/mom/common.nix
    #../../vm-testing.nix                    # Uncomment for VM testing
    ./hardware.nix
    # (disko + "/moms-pc.nix")               # Uncomment for real hardware

    # Desktop environment (combined modules include both system and user config)
    (modules + "/desktop/gnome.nix")
    (modules + "/services/flatpak/base.nix")
    (modules + "/services/ssh.nix")
    (modules + "/tailscale.nix")
    (modules + "/security.nix")
    (modules + "/nix.nix")

    # Optional: gaming support
    # (modules + "/gaming.nix")
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    git vim wget curl
  ];

  system.stateVersion = "25.11";
}
```

**Note:** The `modules` and `disko` variables are path shortcuts provided by `lib/default.nix` via specialArgs.

## Step 5: Create Hardware Configuration

### Option A: VM Testing First (Recommended)

Use this minimal hardware.nix for VM testing before deploying to real hardware:

```nix
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  # VM-compatible defaults
  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
  boot.kernelModules = [ "kvm-intel" "kvm-amd" ];

  # Placeholder - will be replaced with real hardware config later
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
```

### Option B: Real Hardware

On the target machine, generate the hardware config:

```bash
nixos-generate-config --show-hardware-config > users/mom/hosts/moms-pc/hardware.nix
```

## Step 6: Create VM Testing Configuration (Optional)

For VM testing with auto-login, create `users/mom/vm-testing.nix`:

```nix
{ ... }:

{
  # Auto-login for convenience during VM testing
  users.users.mom.initialPassword = "test";
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "mom";

  # VM resources
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
```

Then import it in the host config (remove before deploying to real hardware):

```nix
imports = [
  ../../common.nix
  ../../vm-testing.nix  # Remove for production
  ./hardware.nix
  # ...
];
```

## Step 7: Create the Disko Configuration

Create `disko/moms-pc.nix` (copy from an existing one and adjust disk paths if needed).

**Note:** For VM testing, you can skip disko and comment out the import in the host config.

## Step 8: Add to flake.nix

Edit `flake.nix`:

```nix
nixosConfigurations = {
  laptop = myLib.mkHost {
    hostname = "laptop";
    user = "craig";
  };

  gaming-pc = myLib.mkHost {
    hostname = "gaming-pc";
    user = "craig";
  };

  moms-pc = myLib.mkHost {
    hostname = "moms-pc";
    user = "mom";
  };
};
```

## Step 9: Add Files to Git and Test in VM

```bash
# Add all new files to git
git add users/mom/

# Verify the flake
nix flake check

# Build and run a VM to test
nixos-rebuild build-vm --flake .#moms-pc
./result/bin/run-moms-pc-vm
```

VM login: user `mom`, password `test` (if using vm-testing.nix).

## Step 10: Deploy to Real Hardware

Once the VM works, update for real hardware:

1. Generate real hardware config on the target machine:
   ```bash
   nixos-generate-config --show-hardware-config > users/mom/hosts/moms-pc/hardware.nix
   ```

2. Create disko config if needed: `disko/moms-pc.nix`

3. Remove vm-testing.nix import from host config

4. Deploy:
   ```bash
   sudo nixos-rebuild switch --flake .#moms-pc
   ```

## Available Modules

Modules are organized by domain. **Combined modules** set both NixOS and home-manager options.

### Desktop Modules (import in host config)

| Module | Description | Good for |
|--------|-------------|----------|
| `desktop/gnome.nix` | GDM + GNOME + Pipewire (base system) | Desktop users |
| `desktop/kde.nix` | SDDM + KDE Plasma + Pipewire (base system) | KDE users |

### Combined Modules (import in host config)

| Module | Description | Good for |
|--------|-------------|----------|
| `gaming.nix` | Steam + gamemode + Lutris | Gamers |
| `development.nix` | Podman + libvirt + languages + tools | Developers |
| `security.nix` | Polkit + Yubikey + GPG tools | Security-conscious users |
| `tailscale.nix` | Tailscale service + systray | Remote access |

### Home-Only Modules (import in user's default.nix)

| Module | Description | Good for |
|--------|-------------|----------|
| `shell.nix` | Fish shell, Starship prompt, CLI tools | Power users |
| `git.nix` | Git configuration (generic) | Developers |
| `terminal.nix` | Ghostty terminal | Power users |
| `media.nix` | Music, audio apps | Everyone |
| `editors/vscode.nix` | VS Code + extensions | Developers |
| `editors/neovim.nix` | Neovim config | Vim users |
| `editors/zed.nix` | Zed editor | Developers |
| `zen-browser.nix` | Zen Browser + addons | Everyone |

### System-Only Modules (import in host config)

| Module | Description |
|--------|-------------|
| `nix.nix` | Nix settings, garbage collection |
| `services/ssh.nix` | SSH server |
| `services/vpn.nix` | VPN support |
| `services/flatpak/base.nix` | Base Flatpak apps |

## Quick Reference

**Directory structure for a new user:**
```
users/mom/
├── default.nix      # Home-manager: home-only modules, packages
├── common.nix       # NixOS: users.users.mom, boot loader
├── vm-testing.nix   # VM testing settings (optional)
└── hosts/
    └── moms-pc/
        ├── default.nix   # Imports: common.nix, combined modules, disko
        └── hardware.nix  # VM hardware or generated by nixos-generate-config
```

**flake.nix entry:**
```nix
moms-pc = myLib.mkHost {
  hostname = "moms-pc";
  user = "mom";
};
```

**Host config uses path shortcuts:**
```nix
{ config, pkgs, modules, disko, ... }:
{
  imports = [
    (modules + "/desktop/gnome.nix")
    (disko + "/moms-pc.nix")
  ];
}
```

## Troubleshooting

**"user mom not found"**
- Make sure `users/mom/common.nix` defines `users.users.mom`
- Make sure the host imports `../../common.nix`

**"cannot find users/mom"**
- Create `users/mom/default.nix`
- Add it to git: `git add users/mom/`

**"path does not exist" errors**
- Add new files to git before running `nix flake check`
- Nix flakes only see files tracked by git

**VM build fails with disko errors**
- Comment out the disko import in the host config for VM testing
- Disko is only needed for real hardware installation

**VM runs but screen is black/tiny**
- Increase VM memory in vm-testing.nix: `memorySize = 8192`
- GNOME needs at least 4GB RAM to work properly
