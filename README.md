# Nix Configuration

My personal Nix configuration.

## Prerequisites

- Git
- Gnome

## Installation

### 1. Install Nix

Install Nix using the Determinate Systems installer:

```bash
curl -fsSL https://install.determinate.systems/nix | sh -s -- install --determinate
```

### 2. Clone this repository

```bash
git clone git@github.com:CRBroughton/nix-configuration.git nix-configuration
cd nix-configuration
```

### 3. Apply the Home Manager configuration

```bash
nix run home-manager/master -- switch --flake .#craig -b backup
```

### 4. Install Gnome Extensions

Installs the specified Gnome Extensions in `gnome-extensions-installer.nix` (requires a restart)

```bash
install-gnome-extensions
```


## Usage

This configuration uses [Just](https://github.com/casey/just) as a command runner for common tasks. The `just` command is configured to automatically use this repository's justfile from anywhere.

### Getting Started with Just

View all available commands:
```bash
just
```

All commands support **tab completion** in Fish shell - just type `just` and press TAB to see available commands.

### Quick Examples

```bash
# Apply configuration changes
just switch

# Search and add a package
just search firefox
just add firefox

# Update everything
just update-all

# Clean up old generations
just clean
```

**Note:** Packages added via `just add` are automatically placed in a dedicated section at the bottom of `modules/packages.nix`. I mostly use this when testing out new packages.

## Troubleshooting

### Ghostty won't launch
If you have an NVIDIA GPU, edit `modules/ghostty.nix` and change:
```nix
glWrapper = "nixGLIntel";
```
to:
```nix
glWrapper = "nixGLNvidia";
```

### Flatpak apps not appearing
Run:
```bash
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
```

### VS Code extensions not loading
Extensions are managed immutably by Nix. To modify them, edit `modules/vscode-extensions.nix` and run `home-manager switch`.