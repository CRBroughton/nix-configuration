#!/usr/bin/env bash
# Server Migration Script
# Migrates from nix-home-server to nix-configuration
#
# Usage: Run this script ON THE SERVER as root or with sudo
#   sudo ./migrate-server.sh

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# Configuration
OLD_NIXOS="/etc/nixos"
NEW_NIXOS="/etc/nixos-new"
BACKUP_DIR="/root/nixos-migration-backup"
REPO_URL="https://github.com/CRBroughton/nix-configuration.git"
REPO_BRANCH="nixos-migration"  # Change to 'master' after merging

# Check we're running as root
if [[ $EUID -ne 0 ]]; then
   error "This script must be run as root (use sudo)"
fi

# Check old nixos exists
if [[ ! -d "$OLD_NIXOS" ]]; then
    error "Old NixOS config not found at $OLD_NIXOS"
fi

echo "========================================"
echo "  NixOS Server Migration Script"
echo "========================================"
echo ""
echo "This will migrate from:"
echo "  $OLD_NIXOS (nix-home-server)"
echo "To:"
echo "  $NEW_NIXOS -> $OLD_NIXOS (nix-configuration)"
echo ""
echo "Your data in services/* will be preserved."
echo ""
read -p "Continue? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 0
fi

# Step 1: Stop all containers
log "Stopping all containers..."
cd "$OLD_NIXOS/services"
for dir in */; do
    if [[ -f "${dir}compose.yaml" ]]; then
        log "  Stopping $dir..."
        (cd "$dir" && sudo -u craig podman-compose down 2>/dev/null) || true
    fi
done

# Step 2: Create backup
log "Creating backup at $BACKUP_DIR..."
mkdir -p "$BACKUP_DIR"
tar -czvf "$BACKUP_DIR/services-$(date +%Y%m%d-%H%M%S).tar.gz" \
    "$OLD_NIXOS/services" \
    /var/lib/containers/storage/volumes 2>/dev/null || true

# Also backup secret files
cp /etc/restic-env-password "$BACKUP_DIR/" 2>/dev/null || warn "restic-env-password not found"
cp /etc/restic-env "$BACKUP_DIR/" 2>/dev/null || warn "restic-env not found"

log "Backup complete: $BACKUP_DIR"

# Step 3: Clone new repo
log "Cloning new repository..."
if [[ -d "$NEW_NIXOS" ]]; then
    warn "Removing existing $NEW_NIXOS..."
    rm -rf "$NEW_NIXOS"
fi
git clone --branch "$REPO_BRANCH" "$REPO_URL" "$NEW_NIXOS"

# Step 4: Copy data directories from old services to new
log "Copying service data..."
cd "$OLD_NIXOS/services"
for service in */; do
    service_name="${service%/}"

    # Create service dir in new repo if it doesn't exist
    mkdir -p "$NEW_NIXOS/services/$service_name"

    # Copy data directories (these contain actual data, not tracked by git)
    for datadir in data config tailscale certs .env; do
        src="$OLD_NIXOS/services/$service_name/$datadir"
        dst="$NEW_NIXOS/services/$service_name/"
        if [[ -e "$src" ]]; then
            log "  Copying $service_name/$datadir..."
            cp -a "$src" "$dst"
        fi
    done
done

# Step 5: Swap directories
log "Swapping directories..."
mv "$OLD_NIXOS" "${OLD_NIXOS}-old"
mv "$NEW_NIXOS" "$OLD_NIXOS"

# Step 6: Rebuild NixOS
log "Rebuilding NixOS with new configuration..."
cd "$OLD_NIXOS"
nixos-rebuild switch --flake .#nixos-server

# Step 7: Start containers
log "Starting containers..."
cd "$OLD_NIXOS/services"
for dir in */; do
    if [[ -f "${dir}compose.yaml" ]]; then
        log "  Starting $dir..."
        (cd "$dir" && sudo -u craig podman-compose up -d) || warn "Failed to start $dir"
    fi
done

# Step 8: Verify
log "Verifying containers..."
sudo -u craig podman ps

echo ""
echo "========================================"
echo -e "${GREEN}  Migration Complete!${NC}"
echo "========================================"
echo ""
echo "Old config backed up to: ${OLD_NIXOS}-old"
echo "Data backup at: $BACKUP_DIR"
echo ""
echo "Next steps:"
echo "  1. Verify all services are running: podman ps"
echo "  2. Check service logs: lazydocker"
echo "  3. Test Tailscale access to services"
echo "  4. Once confirmed working, clean up:"
echo "     sudo rm -rf ${OLD_NIXOS}-old"
echo "     sudo rm -rf $BACKUP_DIR"
echo ""
