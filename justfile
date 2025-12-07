# Default recipe - show available commands
default:
    @just --list

# Switch home-manager configuration
switch:
    home-manager switch --flake .#craig

# Switch and show what changed
switch-verbose:
    home-manager switch --flake .#craig --show-trace

# Update all flake inputs (nixpkgs, home-manager, etc.)
update:
    nix flake update
    @echo "Run 'just switch' to apply updates"

# Update and switch in one command
update-all: update switch

# Install GNOME extensions
install-extensions:
    install-gnome-extensions

# Clean old generations (keeps last 7 days)
clean:
    home-manager expire-generations "-7 days"
    nix-collect-garbage --delete-old

# Aggressive clean (removes ALL old generations)
clean-all:
    nix-collect-garbage -d

# Optimize Nix store (deduplicate files to save space)
optimize:
    nix-store --optimise

# Full maintenance: clean and optimize
maintenance: clean optimize

# List all home-manager generations
generations:
    home-manager generations

# Show diff between current and previous generation
diff:
    nix profile diff-closures --profile ~/.local/state/nix/profiles/home-manager

# Check flake for errors
check:
    nix flake check

# Show flake metadata
info:
    nix flake metadata

# Show outdated packages
outdated:
    nix-env -qa --out-path | grep -v '/nix/store'

# Rebuild and create backup
backup:
    home-manager switch --flake .#craig -b backup

# Search for a package
search PACKAGE:
    nix search nixpkgs {{PACKAGE}}

# Show what would be built (dry run)
dry-run:
    home-manager switch --flake .#craig --dry-run

# Format all Nix files
format:
    find . -name '*.nix' -type f -exec nixfmt {} +

# Git commit with conventional commit message
commit MESSAGE:
    git add .
    git commit -m "{{MESSAGE}}"

# Quick commit and push
push MESSAGE: (commit MESSAGE)
    git push

# Check for package updates (Go, Node, Zig)
check-updates:
    @echo "Current versions:"
    @echo "Go: $(go version 2>/dev/null || echo 'not found')"
    @echo "Node: $(node --version 2>/dev/null || echo 'not found')"
    @echo "Zig: $(zig version 2>/dev/null || echo 'not found')"

# Restart GNOME Shell (Wayland: logout required, X11: Alt+F2, r, Enter)
restart-gnome:
    @echo "On Wayland, you need to logout and login again"
    @echo "On X11, press Alt+F2, type 'r', and press Enter"

# Update Flatpaks
update-flatpak:
    flatpak update -y

# List installed Flatpaks
list-flatpak:
    flatpak list --app

# Clean unused Flatpak runtimes
clean-flatpak:
    flatpak uninstall --unused -y

# Run all checks before committing
pre-commit: format check
    @echo "All checks passed! Ready to commit."

# Full system update (Nix + Flatpak)
full-update: update-all update-flatpak
    @echo "System fully updated!"

# Show disk usage of Nix store
disk-usage:
    du -sh /nix/store
    @echo "\nGenerations:"
    @home-manager generations | head -10

# Rollback to previous generation
rollback:
    home-manager generations | head -2 | tail -1 | awk '{print $NF}' | xargs home-manager switch --flake

# Remove a specific generation
remove-generation GEN:
    home-manager remove-generations {{GEN}}