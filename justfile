# Default recipe - show available commands
default:
    @just --list

#═══════════════════════════════════════════════════════════════════════════════
# System Management
#═══════════════════════════════════════════════════════════════════════════════

# Switch NixOS configuration (shows diff + confirmation prompt)
switch:
    nh os switch .

# Switch with trace output for debugging
switch-verbose:
    nh os switch . -- --show-trace

# Test configuration without making it the boot default
test:
    nh os test .

# Build without switching (dry run)
build:
    nh os build .

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
    #!/usr/bin/env bash
    # 1. List and sort numerically
    # 2. Grab the last two entries
    mapfile -t profiles < <(ls -dv /nix/var/nix/profiles/system-*-link | tail -n 2)

    # Check if we actually found two profiles to compare
    if [ ${#profiles[@]} -lt 2 ]; then
        echo "Error: Need at least two system generations to compare."
        exit 1
    fi

    # Run the actual diff
    nvd diff "${profiles[0]}" "${profiles[1]}"

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
    statix fix
    deadnix --edit .
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

# Build VM for brighton-pc config
vm-brighton:
    nix build .#nixosConfigurations.brighton-pc.config.system.build.vm
    ./result/bin/run-brighton-pc-vm

# Build VM for mum-pc config
vm-mum:
    nix build .#nixosConfigurations.mum-pc.config.system.build.vm
    ./result/bin/run-mum-pc-vm

# Build VM for mums-laptop config
vm-mums-laptop:
    nix build .#nixosConfigurations.mums-laptop.config.system.build.vm
    ./result/bin/run-mums-laptop-vm

# Build VM for moons-pc config
vm-moons-pc:
    nix build .#nixosConfigurations.moons-pc.config.system.build.vm
    ./result/bin/run-moons-pc-vm

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

install-brighton-pc:
    sudo nixos-install --flake .#brighton-pc

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

#═══════════════════════════════════════════════════════════════════════════════
# Server / Containers (run on nixos-server)
#═══════════════════════════════════════════════════════════════════════════════

# Start all podman services
up:
    #!/usr/bin/env bash
    cd /etc/nixos/services
    for compose in */compose.yaml; do
      echo "Starting $(dirname $compose)..."
      podman-compose -f "$compose" up -d
    done

# Stop all podman services
down:
    #!/usr/bin/env bash
    cd /etc/nixos/services
    for compose in */compose.yaml; do
      echo "Stopping $(dirname $compose)..."
      podman-compose -f "$compose" down
    done

# Restart all podman services
restart: down up

# Start a specific service (e.g., just start adguard)
start service:
    cd /etc/nixos/services/{{service}} && podman-compose up -d

# Stop a specific service
stop service:
    cd /etc/nixos/services/{{service}} && podman-compose down

# View logs for a service
logs service:
    cd /etc/nixos/services/{{service}} && podman-compose logs -f

# Pull latest images for all services
pull:
    #!/usr/bin/env bash
    cd /etc/nixos/services
    for compose in */compose.yaml; do
      echo "Pulling $(dirname $compose)..."
      podman-compose -f "$compose" pull
    done

# Add The Lounge user
thelounge-adduser username:
    podman exec -it thelounge thelounge add {{username}}

# Generate IRC operator password
irc-genpasswd:
    podman exec -it ergo /ircd-bin/ergo genpasswd

# Add XMPP user (e.g., just xmpp-adduser craig)
xmpp-adduser username:
    podman exec -it prosody prosodyctl adduser {{username}}@xmpp.tail538465.ts.net

# Generate XMPP Tailscale certificate
xmpp-gencert:
    podman exec tailscale-xmpp tailscale cert --cert-file /certs/xmpp.tail538465.ts.net.crt --key-file /certs/xmpp.tail538465.ts.net.key xmpp.tail538465.ts.net
    @echo "Certificate generated. Restart Prosody: podman restart prosody"

# Fix container data permissions (e.g., just fix-perms xmpp 100 102)
fix-perms service uid gid:
    cd /etc/nixos/services/{{service}} && podman unshare chown -R {{uid}}:{{gid}} data/
    @echo "Fixed permissions for {{service}} (uid={{uid}}, gid={{gid}})"

# Trust /etc/nixos git repo for root (fixes ownership errors)
fix-git-ownership:
    sudo git config --global --add safe.directory /etc/nixos

#═══════════════════════════════════════════════════════════════════════════════
# Raspberry Pi
#═══════════════════════════════════════════════════════════════════════════════

# Build Pi SD card image
build-pi:
    nix build .#images.pi-monitor
    @echo "Image built: result/sd-image/"
    @ls -lh result/sd-image/

# Flash Pi to SD card (e.g., just flash-pi /dev/sdb)
flash-pi device:
    #!/usr/bin/env bash
    set -e
    img=$(find result/sd-image -name "*.img.zst" | head -1)
    echo "Flashing $img to {{device}}..."
    zstd -dc "$img" | sudo dd of={{device}} bs=4M status=progress conv=fsync
    sync
    echo "Done! Remove SD card and boot your Pi."

# Deploy to Pi (from another machine)
deploy-pi:
    nixos-rebuild switch --flake .#pi-monitor --target-host craig@pi-monitor --build-host localhost --use-remote-sudo

# Deploy to server (from another machine)
deploy-server:
    nixos-rebuild switch --flake .#nixos-server --target-host craig@nixos-server --use-remote-sudo
