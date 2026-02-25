# NixOS Configuration

NixOS configuration for my machines: laptop, gaming PC, and more.

## Structure

```
nix-configuration/
├── flake.nix                      # Entry point - defines inputs and hosts
├── lib/
│   └── default.nix                # Helper functions (mkHost)
├── hosts/                         # Machine-specific configurations
│   ├── laptop/
│   │   ├── default.nix            # Laptop config (kernel, user, VM settings)
│   │   └── hardware.nix           # Hardware-specific settings
│   └── gaming-pc/
│       ├── default.nix
│       └── hardware.nix
├── modules/                       # Reusable, self-contained modules
│   ├── nixos/                     # NixOS system modules
│   │   ├── desktop/gnome.nix      # GNOME desktop + audio + fonts
│   │   ├── gaming.nix             # Steam, gamemode, gamescope
│   │   ├── virtualisation.nix     # Podman, libvirt, virt-manager
│   │   ├── security.nix           # Polkit, Yubikey support
│   │   ├── nix.nix                # Nix settings, garbage collection
│   │   └── services/              # System services
│   │       ├── flatpak.nix        # Declarative Flatpak packages
│   │       ├── ssh.nix
│   │       ├── tailscale.nix
│   │       └── printing.nix
│   └── home-manager/              # Home Manager modules
│       ├── shell.nix              # Fish, Starship, Zoxide, CLI tools
│       ├── git.nix                # Git, Jujutsu, SSH keys
│       ├── terminal.nix           # Ghostty terminal
│       ├── gnome.nix              # dconf settings, GNOME extensions
│       ├── development.nix        # Languages and dev tools
│       ├── gaming.nix             # Lutris
│       ├── media.nix              # Audio/communication apps
│       ├── security.nix           # Yubikey tools, Kleopatra
│       └── editors/               # Code editors
│           ├── vscode.nix
│           ├── neovim.nix
│           └── zed.nix
├── profiles/                      # Composable feature bundles
│   ├── workstation.nix            # Full desktop + gaming + development
│   ├── desktop.nix                # GNOME + flatpak + printing
│   ├── gaming.nix                 # Gaming support
│   ├── development.nix            # Dev tools + containers
│   └── server.nix                 # Headless (SSH, Tailscale)
├── users/                         # User-specific Home Manager configs
│   └── craig/
│       └── default.nix
├── disko/                         # Declarative disk partitioning
│   ├── laptop.nix
│   └── gaming-pc.nix
├── config/                        # External config files
│   └── neovim/                    # Neovim configuration
├── xkb/                           # Custom keyboard layouts
└── fonts/                         # Custom fonts
```

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

**System packages** (available to all users, requires sudo):
Edit `modules/nixos/` files or add to `environment.systemPackages` in a profile.

**User packages** (home-manager):
Edit the appropriate module in `modules/home-manager/`:

| Package Type | File |
|-------------|------|
| CLI tools | `modules/home-manager/shell.nix` |
| Dev languages/tools | `modules/home-manager/development.nix` |
| Gaming | `modules/home-manager/gaming.nix` |
| Media/audio apps | `modules/home-manager/media.nix` |
| Security/Yubikey | `modules/home-manager/security.nix` |

Example - adding `htop`:
```nix
# modules/home-manager/shell.nix
home.packages = with pkgs; [
  bat
  eza
  htop  # Add here
  ...
];
```

Then run `just switch`.

**Flatpak apps**:
Edit `modules/nixos/services/flatpak.nix`:
```nix
packages = [
  "com.spotify.Client"  # Add Flatpak app ID
  ...
];
```

### Adding VSCode Extensions

Edit `modules/home-manager/editors/vscode.nix`:

```nix
extensions = with pkgs.vscode-marketplace; [
  publisher.extension-name  # Add extension
  ...
];
```

Find extension IDs on the VS Code marketplace (format: `publisher.extension-name`).

### Changing GNOME Settings

Edit `modules/home-manager/gnome.nix`:

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

1. Add the extension package to `modules/home-manager/gnome.nix`:
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

Edit `modules/home-manager/gnome.nix`:

```nix
favorite-apps = [
  "app.zen_browser.zen.desktop"
  "org.gnome.Nautilus.desktop"
  # Add/remove/reorder apps here
];
```

Find `.desktop` file names in `/run/current-system/sw/share/applications/` or `~/.local/share/flatpak/exports/share/applications/`.

### Creating a New Module

1. Create the file in the appropriate location:
   - System module: `modules/nixos/<name>.nix`
   - Home module: `modules/home-manager/<name>.nix`

2. Basic module structure:
   ```nix
   { config, pkgs, ... }:

   {
     # Your configuration here
   }
   ```

3. Import it in a profile or user config:
   ```nix
   imports = [
     ../modules/home-manager/<name>.nix
   ];
   ```

### Adding a New Machine

1. Create host directory:
   ```bash
   mkdir -p hosts/<hostname>
   ```

2. Generate hardware config (on the target machine):
   ```bash
   nixos-generate-config --show-hardware-config > hosts/<hostname>/hardware.nix
   ```

3. Create `hosts/<hostname>/default.nix`:
   ```nix
   { config, pkgs, ... }:

   {
     imports = [
       ./hardware.nix
       ../../profiles/workstation.nix  # or server.nix, etc.
     ];

     boot.loader.systemd-boot.enable = true;
     boot.loader.efi.canTouchEfiVariables = true;
     boot.kernelPackages = pkgs.linuxPackages_zen;

     users.users.craig = {
       isNormalUser = true;
       extraGroups = [ "wheel" "networkmanager" ];
       shell = pkgs.fish;
     };

     system.stateVersion = "25.11";
   }
   ```

4. Create `disko/<hostname>.nix` for disk partitioning.

5. Add to `flake.nix`:
   ```nix
   <hostname> = myLib.mkHost { hostname = "<hostname>"; };
   ```

### Adding a New Desktop Environment

1. Create `modules/nixos/desktop/<name>.nix`:
   ```nix
   { config, pkgs, ... }:

   {
     # Desktop environment config
     programs.<name>.enable = true;

     # Audio (copy from gnome.nix)
     services.pipewire = { ... };

     # Fonts (copy from gnome.nix)
     fonts.packages = [ ... ];
   }
   ```

2. Create a new profile or modify `profiles/desktop.nix`.

3. Create corresponding home-manager module for user settings.

### Testing Changes Safely

Test in a VM before deploying to real hardware:

```bash
just vm          # Build and run laptop VM
just vm-gaming   # Build and run gaming-pc VM
```

VM login: user `craig`, password `test` (configured for VM only).

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

1. Boot NixOS installer USB
2. Connect to network
3. Clone this repo:
   ```bash
   nix-shell -p git
   git clone https://github.com/CRBroughton/nix-configuration.git
   cd nix-configuration
   ```
4. Partition disk:
   ```bash
   just disko-laptop device="/dev/nvme0n1"
   ```
5. Install:
   ```bash
   just install-laptop
   ```
6. Reboot and copy your SSH private key to `~/.ssh/id_ed25519`

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

Extensions are managed by Nix. Edit `modules/home-manager/editors/vscode.nix` and run `just switch`.

### Finding package names

```bash
nix search nixpkgs <name>
```

Or browse https://search.nixos.org/packages
