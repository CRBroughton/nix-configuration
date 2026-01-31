# Default recipe - show available commands
default:
    @just --list

# Switch home-manager configuration
switch:
    home-manager switch --flake .

# Switch and show what changed
switch-verbose:
    home-manager switch --flake . --show-trace

# Update all flake inputs (nixpkgs, home-manager, etc.)
update:
    nix flake update
    @echo "Run 'just switch' to apply updates"

# Update and switch in one command
update-all: update switch

# Clean old generations (keeps last 7 days)
clean:
    home-manager expire-generations "-7 days"
    nix-collect-garbage --delete-old

# Aggressive clean (removes ALL old generations)
clean-all:
    nix-collect-garbage -d

# Optimise Nix store (deduplicate files to save space)
optimise:
    nix-store --optimise

# Full maintenance: clean and optimise
maintenance: clean optimise

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
    home-manager switch --flake . -b backup

# Search for a package using nix-search-cli
search PACKAGE:
    nix-search {{PACKAGE}}

# Add a package to a packages file
add PACKAGE:
    #!/usr/bin/env bash
    set -e

    # Package files with friendly names
    declare -A FILES=(
        ["1"]="modules/packages/cli.nix:CLI & Utilities"
        ["2"]="modules/packages/programming.nix:Programming"
        ["3"]="modules/packages/gaming.nix:Gaming"
        ["4"]="modules/packages/audio.nix:Audio"
        ["5"]="modules/packages/yubikey.nix:YubiKey"
        ["6"]="modules/packages/package-management.nix:Package Management"
    )

    echo "Select a category for '{{PACKAGE}}':"
    echo ""
    for key in $(echo "${!FILES[@]}" | tr ' ' '\n' | sort -n); do
        IFS=':' read -r path name <<< "${FILES[$key]}"
        echo "  $key) $name"
    done
    echo ""
    read -p "Choice [1-6]: " choice

    if [[ -z "${FILES[$choice]}" ]]; then
        echo "Invalid selection"
        exit 1
    fi

    IFS=':' read -r PACKAGES_FILE category <<< "${FILES[$choice]}"

    # Check if package already exists in any packages file
    for key in "${!FILES[@]}"; do
        IFS=':' read -r path _ <<< "${FILES[$key]}"
        if grep -q "^\s*{{PACKAGE}}\s*$" "$path"; then
            echo "Package '{{PACKAGE}}' already exists in $path"
            exit 1
        fi
    done

    # Insert before the closing ]; of the array
    sed -i '/^  \];$/i\    {{PACKAGE}}' "$PACKAGES_FILE"

    echo "✓ Added '{{PACKAGE}}' to $category ($PACKAGES_FILE)"
    echo "Run 'just switch' to install the package"

# Remove a package from packages files
remove PACKAGE:
    #!/usr/bin/env bash
    set -e

    PACKAGE_FILES=(
        "modules/packages/cli.nix"
        "modules/packages/programming.nix"
        "modules/packages/gaming.nix"
        "modules/packages/audio.nix"
        "modules/packages/yubikey.nix"
        "modules/packages/package-management.nix"
    )

    FOUND=""
    for file in "${PACKAGE_FILES[@]}"; do
        if grep -q "^\s*{{PACKAGE}}\s*$" "$file"; then
            FOUND="$file"
            break
        fi
    done

    if [[ -z "$FOUND" ]]; then
        echo "Package '{{PACKAGE}}' not found in any packages file"
        exit 1
    fi

    # Remove the package line
    sed -i "/^\s*{{PACKAGE}}\s*$/d" "$FOUND"

    echo "✓ Removed '{{PACKAGE}}' from $FOUND"
    echo "Run 'just switch' to uninstall the package"

# Search and add a package interactively
search-add QUERY:
    #!/usr/bin/env bash
    echo "Searching for packages matching '{{QUERY}}'..."
    nix-search '{{QUERY}}'
    echo ""
    read -p "Enter the exact package name to add (or press Enter to cancel): " PACKAGE
    if [ -n "$PACKAGE" ]; then
        just add "$PACKAGE"
        echo ""
        read -p "Install now? (Y/n): " INSTALL
        if [[ -z "$INSTALL" || "$INSTALL" =~ ^[Yy]$ ]]; then
            just switch
        else
            echo "Package added. Run 'just switch' to install it."
        fi
    else
        echo "Cancelled."
    fi

# Show what would be built (dry run)
dry-run:
    home-manager switch --flake . --dry-run

# Format all Nix files
format:
    #!/usr/bin/env bash
    if ! command -v nixfmt &> /dev/null; then
        echo "Error: nixfmt not found. Run 'just switch' first to install it."
        exit 1
    fi
    find . -name '*.nix' -type f -exec nixfmt {} +
    echo "✓ Formatted all Nix files"

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

# Update CachyOS system packages
update-cachyos:
    sudo pacman -Syu

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

# Show Nix configuration report
report:
    #!/usr/bin/env bash
    echo "=== Nix Configuration Report ==="
    echo ""
    echo "Current Generation:"
    home-manager generations | head -1
    echo ""
    echo "Installed Packages:"
    nix-store -q --references ~/.nix-profile | wc -l
    echo ""
    echo "Nix Store Disk Usage:"
    du -sh /nix/store
    echo ""
    echo "Home Manager Generations:"
    home-manager generations | wc -l
    echo ""
    echo "Recent Generations:"
    home-manager generations | head -5

# Initialize Lua project with .luarc.json
init-lua:
    lua scripts/init-lua.lua