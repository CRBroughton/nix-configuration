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
cd ~/code
git clone <repository-url> nix-configuration
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

### Updating the configuration

After making changes to any `.nix` files, apply them with:

```bash
home-manager switch --flake .#craig
```

### Updating all packages

```bash
nix flake update
home-manager switch --flake .#craig
```

### Removing unused packages

To clean up old generations and free up disk space:

```bash
# Remove old home-manager generations (keeps last 7 days)
home-manager expire-generations "-7 days"

# Collect garbage and remove unused packages
nix-collect-garbage --delete-old

# Or be more aggressive (remove everything not currently in use, including all previous generations)
nix-collect-garbage -d

# Optimize the Nix store (deduplicate files)
nix-store --optimise
```

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