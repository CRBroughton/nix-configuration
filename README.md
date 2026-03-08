# NixOS Configuration

NixOS configuration for my machines: laptop, gaming PC, and more.

## Structure

```
nix-configuration/
├── flake.nix                      # Entry point - defines inputs and hosts
├── lib/
│   └── default.nix                # Helper functions (mkHost), path shortcuts
├── users/                         # User configs + their machines
│   └── craig/
│       ├── default.nix            # Home-manager config (shell, editors, etc.)
│       ├── git.nix                # Personal git config (name, email, keys)
│       ├── flatpaks.nix           # Personal Flatpak apps
│       ├── common.nix             # System user config (users.users.craig)
│       ├── vm-testing.nix         # VM testing settings
│       └── hosts/                 # Craig's machines
│           ├── laptop/
│           │   ├── default.nix    # Laptop config (imports modules directly)
│           │   └── hardware.nix
│           └── gaming-pc/
│               ├── default.nix
│               └── hardware.nix
├── modules/                       # Domain-based modules
│   ├── gaming.nix                 # Steam + gamemode + Lutris (combined)
│   ├── security.nix               # Polkit + Yubikey (combined)
│   ├── development.nix            # Podman + libvirt + dev tools (combined)
│   ├── tailscale.nix              # Tailscale service + systray (combined)
│   ├── nix.nix                    # Nix settings, garbage collection
│   ├── shell.nix                  # Fish, Starship, Zoxide, CLI tools
│   ├── terminal.nix               # Ghostty terminal
│   ├── git.nix                    # Git config (generic)
│   ├── media.nix                  # Audio/communication apps
│   ├── zen-browser.nix            # Zen Browser + addons
│   ├── desktop/
│   │   ├── gnome.nix              # GDM + GNOME + theme + extensions (combined)
│   │   └── kde.nix                # KDE Plasma desktop
│   ├── services/
│   │   ├── ssh.nix
│   │   ├── printing.nix
│   │   ├── vpn.nix
│   │   └── flatpak/base.nix       # Base Flatpak apps
│   └── editors/
│       ├── vscode.nix
│       ├── neovim.nix
│       └── zed.nix
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
| Services | SSH, Tailscale, VPN, CUPS printing |
| Security | Polkit, Yubikey support, GPG tools |
| Extras | Power profiles daemon, fwupd |

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
| Services | SSH, Tailscale, VPN, CUPS printing |
| Security | Polkit, Yubikey support, GPG tools |
| Extras | fwupd |

Both hosts share the same user config (craig) with identical home-manager modules.

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

**System packages** (available to all users):
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
Personal flatpaks: `users/craig/flatpaks.nix`

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

Edit `modules/desktop/gnome.nix` (in the home-manager section):

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

In `modules/desktop/gnome.nix`, add to the home-manager section:

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

Edit `modules/desktop/gnome.nix` (in the dconf.settings):

```nix
favorite-apps = [
  "app.zen_browser.zen.desktop"
  "org.gnome.Nautilus.desktop"
  # Add/remove/reorder apps here
];
```

Find `.desktop` file names in `/run/current-system/sw/share/applications/` or `~/.local/share/flatpak/exports/share/applications/`.

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

3. Import it in a host config using the `modules` path shortcut:
   ```nix
   imports = [
     (modules + "/<name>.nix")
   ];
   ```

### Adding a New Machine

See [docs/adding-new-user.md](docs/adding-new-user.md) for the complete guide.

**Quick summary:**

1. Create the user directory structure:
   ```bash
   mkdir -p users/<username>/hosts/<hostname>
   ```

2. Create the required files:
   - `users/<username>/default.nix` - Home-manager config
   - `users/<username>/common.nix` - System user config
   - `users/<username>/hosts/<hostname>/default.nix` - Host config
   - `users/<username>/hosts/<hostname>/hardware.nix` - Hardware config

3. Add to `flake.nix`:
   ```nix
   <hostname> = myLib.mkHost {
     hostname = "<hostname>";
     user = "<username>";
   };
   ```

### Testing Changes Safely

Test in a VM before deploying to real hardware:

```bash
just vm          # Build and run laptop VM
just vm-gaming   # Build and run gaming-pc VM
```

VM login: user `craig`, password `test` (set via `vm-testing.nix`).

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
   nixos-generate-config --root /mnt --show-hardware-config > users/craig/hosts/laptop/hardware.nix
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
