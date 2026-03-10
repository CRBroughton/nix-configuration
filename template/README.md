# NixOS Configuration Template

A minimal, barebones NixOS configuration template. No desktop environment, no custom shell, no opinionated defaults — just the structure to build from.

## What you get out of the box

This template gives you a working NixOS system with:

- A bootable system (systemd-boot, EFI)
- NetworkManager for networking
- A single user with `wheel` (sudo) access
- `git`, `vim`, `curl`, `wget` as basic system packages
- Home-manager wired up and ready for user-level config
- A VM target so you can test changes before touching real hardware
- A `justfile` with common commands (`switch`, `update`, `vm`, `rollback`, `clean`)

What it intentionally leaves out:

- No desktop environment
- No custom shell (defaults to bash)
- No theming, fonts, or visual config
- No developer tools, gaming, or media packages
- No additional services

Everything beyond the basics is opt-in — you add what you need, nothing more.

## Why this approach?

### Flake-based

All inputs (nixpkgs, home-manager) are pinned in `flake.lock`. This means the system is fully reproducible — you can rebuild the exact same system on any machine, at any point in the future, and get the same result. No surprise updates breaking things.

### User/host separation

Config is split by user and machine under `users/<name>/hosts/<hostname>/`. This keeps things tidy when you have more than one machine:

- User-level config (shell, editor, packages) lives under `users/yourname/`
- Machine-level config (hardware, services, firewall) lives under `users/yourname/hosts/yourhostname/`
- Shared user config automatically applies to every machine that user has

### Home-manager included

[Home-manager](https://github.com/nix-community/home-manager) manages user-level configuration declaratively — dotfiles, shell config, editor settings, user packages. It's wired up from the start so you don't have to integrate it yourself later.

### `mkHost` helper

Rather than duplicating boilerplate in every host config, `lib/default.nix` provides a `mkHost` function that wires up nixpkgs, home-manager, and your host config in one call. Adding a new machine is just adding a few lines to `flake.nix`.

### VM-first workflow

The `vm-testing.nix` file and `just vm` command let you test any change in a VM before applying it to real hardware. This is the safest way to experiment — especially useful when learning NixOS or making significant changes.

For a fuller example of this structure in action — with multiple machines, desktop environments, server config, and more — see the [full configuration](https://github.com/crbroughton/nix-configuration) this template is based on.

## Structure

```
template/
├── flake.nix                              # Entry point
├── justfile                               # Common commands
├── lib/
│   └── default.nix                        # mkHost helper
└── users/
    └── username/                          # Your user
        ├── default.nix                    # Home-manager config
        └── hosts/
            └── hostname/                  # Your machine
                ├── default.nix            # Host config
                ├── hardware.nix           # Hardware config (generated)
                └── vm-testing.nix         # VM settings (remove for real hardware)
```

## Getting Started

### 1. Rename placeholders

Replace `username` and `hostname` throughout:

```bash
# Rename directories
mv users/username users/yourname
mv users/yourname/hosts/hostname users/yourname/hosts/yourhostname
```

Then update these files:

| File | What to change |
|------|---------------|
| `flake.nix` | Both occurrences of `hostname` and `user = "username"` |
| `users/yourname/hosts/yourhostname/default.nix` | `username` in `users.users.username` |
| `users/yourname/hosts/yourhostname/vm-testing.nix` | `username` in `users.users.username.initialPassword` |
| `justfile` | `hostname` in the `vm` command |

### 2. Generate hardware config

On your target machine (or NixOS installer):

```bash
nixos-generate-config --root /mnt --show-hardware-config > users/yourname/hosts/yourhostname/hardware.nix
```

Or on an already-running NixOS system:

```bash
nixos-generate-config --show-hardware-config > users/yourname/hosts/yourhostname/hardware.nix
```

### 3. Test in a VM first

```bash
just vm
```

Login with user `username` (or whatever you renamed it to), password `test`.

### 4. Install on real hardware

Boot the NixOS installer, then:

```bash
nix-shell -p git just
git clone <your-repo> /mnt/etc/nixos
cd /mnt/etc/nixos

# Partition your disk (manual or with disko)
# Then install:
sudo nixos-install --flake .#yourhostname
```

Remove the `vm-testing.nix` import from `default.nix` before deploying to real hardware.

### 5. Apply changes

Once installed and booted:

```bash
just switch
```

---

## Expanding the Configuration

### Adding packages

**System-wide** (all users on this machine):

```nix
# users/yourname/hosts/yourhostname/default.nix
environment.systemPackages = with pkgs; [
  git
  vim
  your-package
];
```

**User packages** via home-manager:

```nix
# users/yourname/default.nix
home.packages = with pkgs; [
  your-package
];
```

### Adding a desktop environment

#### GNOME

```nix
# users/yourname/hosts/yourhostname/default.nix
services.xserver.enable = true;
services.displayManager.gdm.enable = true;
services.desktopManager.gnome.enable = true;
services.xserver.xkb.layout = "gb,us";  # Set your keyboard layout

# Remove unwanted GNOME apps
environment.gnome.excludePackages = with pkgs; [
  gnome-tour
  gnome-music
  epiphany
];

# Audio (Pipewire)
services.pulseaudio.enable = false;
security.rtkit.enable = true;
services.pipewire = {
  enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
  pulse.enable = true;
};

# Fonts with emoji support
fonts.packages = with pkgs; [
  noto-fonts
  noto-fonts-color-emoji
];
```

For a more complete GNOME setup including personal settings (themes, dock, extensions, dconf), see [`modules/desktop/gnome.nix`](https://github.com/crbroughton/nix-configuration/blob/master/modules/desktop/gnome.nix) and [`users/craig/gnome.nix`](https://github.com/crbroughton/nix-configuration/blob/master/users/craig/gnome.nix) in the main repo.

#### KDE Plasma

```nix
# users/yourname/hosts/yourhostname/default.nix
services.xserver.enable = true;
services.displayManager.sddm.enable = true;
services.displayManager.sddm.wayland.enable = true;
services.desktopManager.plasma6.enable = true;
services.xserver.xkb.layout = "gb,us";  # Set your keyboard layout

# Remove unwanted KDE apps
environment.plasma6.excludePackages = with pkgs.kdePackages; [
  elisa
];

# Audio (Pipewire)
services.pulseaudio.enable = false;
security.rtkit.enable = true;
services.pipewire = {
  enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
  pulse.enable = true;
};

# Fonts with emoji support
fonts.packages = with pkgs; [
  noto-fonts
  noto-fonts-color-emoji
];
```

For a more complete KDE setup, see [`modules/desktop/kde.nix`](https://github.com/crbroughton/nix-configuration/blob/master/modules/desktop/kde.nix) in the main repo.

#### Tip: extract into a module

Rather than putting all of this in your host config, create a dedicated module:

```nix
# modules/desktop/gnome.nix
{ pkgs, ... }:
{
  services.xserver.enable = true;
  services.displayManager.gdm.enable = true;
  # ... rest of config
}
```

Then import it in your host:

```nix
# users/yourname/hosts/yourhostname/default.nix
imports = [
  ./hardware.nix
  ../../modules/desktop/gnome.nix
];
```

### Adding a custom shell (Fish)

```nix
# default.nix - enable system-wide
programs.fish.enable = true;

# Then set it as the user's shell
users.users.yourname.shell = pkgs.fish;
```

### Adding home-manager modules

Create a module file and import it:

```nix
# users/yourname/default.nix
{ pkgs, ... }:
{
  imports = [ ./shell.nix ];
  home.stateVersion = "25.11";
}
```

```nix
# users/yourname/shell.nix
{ pkgs, ... }:
{
  programs.fish.enable = true;
  home.packages = with pkgs; [ eza bat fzf ];
}
```

### Adding another machine

```
users/yourname/hosts/
├── hostname/          # existing
└── new-machine/       # new
    ├── default.nix
    ├── hardware.nix
    └── vm-testing.nix
```

Add it to `flake.nix`:

```nix
nixosConfigurations = {
  hostname = myLib.mkHost { ... };
  new-machine = myLib.mkHost {
    hostname = "new-machine";
    user = "yourname";
  };
};
```

### Sharing config between machines

Create a shared module:

```nix
# users/yourname/hosts/common.nix
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ git vim curl ];
}
```

Import it in each host's `default.nix`:

```nix
imports = [
  ./hardware.nix
  ../common.nix  # shared between all your machines
];
```

---

## Dev Shell

The flake includes a dev shell with Nix tooling pre-configured:

```bash
nix develop
```

This gives you:

| Tool | Purpose |
|------|---------|
| `nixfmt` | Format Nix files |
| `statix` | Lint for anti-patterns |
| `deadnix` | Find unused code |
| `nixd` / `nil` | Nix language servers (for editor integration) |
| `nix-format` | Format all files in one command |

Run `nix-format` before committing to keep files consistently formatted.

## Common Commands

| Command | Description |
|---------|-------------|
| `just switch` | Apply configuration changes |
| `just switch-verbose` | Apply with full error output |
| `just update` | Update flake inputs (nixpkgs, etc.) |
| `just update-all` | Update + apply in one step |
| `just vm` | Build and run in a VM |
| `just check` | Check for errors without building |
| `just rollback` | Revert to previous generation |
| `just clean` | Remove old generations (30d+) |
| `just maintenance` | Clean + optimise store |

---

## VM Notes

The `vm-testing.nix` file configures the VM with:
- 4GB RAM, 4 cores, 20GB disk
- KVM acceleration (requires KVM on the host)
- Password set to `test` for easy login

Remove the `vm-testing.nix` import from `default.nix` before installing on real hardware — you don't want a world-readable hardcoded password on a real system.
