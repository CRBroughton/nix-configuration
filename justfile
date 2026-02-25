# Default recipe - show available commands
default:
    @just --list

#═══════════════════════════════════════════════════════════════════════════════
# System Management
#═══════════════════════════════════════════════════════════════════════════════

# Switch NixOS configuration
switch:
    sudo nixos-rebuild switch --flake .

# Switch with trace output for debugging
switch-verbose:
    sudo nixos-rebuild switch --flake . --show-trace

# Test configuration without making it the boot default
test:
    sudo nixos-rebuild test --flake .

# Build without switching (dry run)
build:
    nixos-rebuild build --flake .

# Update all flake inputs
update:
    nix flake update
    @echo "Run 'just switch' to apply updates"

# Update and switch in one command
update-all: update switch

#═══════════════════════════════════════════════════════════════════════════════
# Maintenance
#═══════════════════════════════════════════════════════════════════════════════

# Clean old generations (keeps last 30 days)
clean:
    sudo nix-collect-garbage --delete-older-than 30d

# Aggressive clean (removes ALL old generations)
clean-all:
    sudo nix-collect-garbage -d

# Optimise Nix store (deduplicate files to save space)
optimise:
    sudo nix-store --optimise

# Full maintenance: clean and optimise
maintenance: clean optimise

# List all system generations
generations:
    sudo nix-env --list-generations --profile /nix/var/nix/profiles/system

# Show diff between current and previous generation
diff:
    nvd diff /run/current-system result

#═══════════════════════════════════════════════════════════════════════════════
# Flake Management
#═══════════════════════════════════════════════════════════════════════════════

# Check flake for errors
check:
    nix flake check

# Show flake metadata
info:
    nix flake metadata

# Show flake inputs
inputs:
    nix flake metadata --json | jq '.locks.nodes | keys'

# Format all Nix files
format:
    find . -name '*.nix' -type f -exec nixfmt {} +
    @echo "Formatted all Nix files"

#═══════════════════════════════════════════════════════════════════════════════
# VM Testing
#═══════════════════════════════════════════════════════════════════════════════

# Build and run VM for testing (laptop config)
vm:
    nix build .#nixosConfigurations.laptop.config.system.build.vm
    ./result/bin/run-laptop-vm

# Build VM for gaming-pc config
vm-gaming:
    nix build .#nixosConfigurations.gaming-pc.config.system.build.vm
    ./result/bin/run-gaming-pc-vm

#═══════════════════════════════════════════════════════════════════════════════
# Installation (run from NixOS installer)
#═══════════════════════════════════════════════════════════════════════════════

# Partition disk with disko (laptop)
disko-laptop device="/dev/nvme0n1":
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko ./disko/laptop.nix --arg device '"{{device}}"'

# Partition disk with disko (gaming-pc)
disko-gaming device="/dev/nvme0n1":
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko ./disko/gaming-pc.nix --arg device '"{{device}}"'

# Install NixOS (laptop)
install-laptop:
    sudo nixos-install --flake .#laptop

# Install NixOS (gaming-pc)
install-gaming:
    sudo nixos-install --flake .#gaming-pc

#═══════════════════════════════════════════════════════════════════════════════
# VPN
#═══════════════════════════════════════════════════════════════════════════════

# List available PIA VPN regions
pia-list:
    @ls /etc/openvpn/pia/*.ovpn 2>/dev/null | xargs -n1 basename | sed 's/.ovpn//' || echo "PIA configs not found"

# Import a PIA VPN region (e.g., just pia-import uk_london)
pia-import region:
    nmcli connection import type openvpn file /etc/openvpn/pia/{{region}}.ovpn

# Connect to a VPN
vpn-connect name:
    nmcli connection up {{name}}

# Disconnect VPN
vpn-disconnect name:
    nmcli connection down {{name}}

# Show VPN status
vpn-status:
    @nmcli connection show --active | grep -i vpn || echo "No active VPN"

#═══════════════════════════════════════════════════════════════════════════════
# Flatpak
#═══════════════════════════════════════════════════════════════════════════════

# Update Flatpaks
update-flatpak:
    flatpak update -y

# List installed Flatpaks
list-flatpak:
    flatpak list --app

# Clean unused Flatpak runtimes
clean-flatpak:
    flatpak uninstall --unused -y

#═══════════════════════════════════════════════════════════════════════════════
# Git
#═══════════════════════════════════════════════════════════════════════════════

# Git commit with message
commit MESSAGE:
    git add .
    git commit -m "{{MESSAGE}}"

# Quick commit and push
push MESSAGE: (commit MESSAGE)
    git push

# Run checks before committing
pre-commit: format check
    @echo "All checks passed! Ready to commit."

#═══════════════════════════════════════════════════════════════════════════════
# Info
#═══════════════════════════════════════════════════════════════════════════════

# Show NixOS configuration report
report:
    #!/usr/bin/env bash
    echo "=== NixOS Configuration Report ==="
    echo ""
    echo "System:"
    nixos-version
    echo ""
    echo "Kernel:"
    uname -r
    echo ""
    echo "Current Generation:"
    sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | tail -1
    echo ""
    echo "Nix Store Disk Usage:"
    du -sh /nix/store
    echo ""
    echo "System Generations:"
    sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | wc -l

# Show disk usage
disk-usage:
    du -sh /nix/store
    @echo ""
    @echo "Generations:"
    @sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | head -10

# Rollback to previous generation
rollback:
    sudo nixos-rebuild switch --rollback
