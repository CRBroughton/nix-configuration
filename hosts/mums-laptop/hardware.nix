# Hardware configuration - replace this with your generated hardware config
# Generate with: nixos-generate-config --root /mnt --show-hardware-config > hardware.nix
_:

{
  # Placeholder root filesystem - replace with output of nixos-generate-config
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
}
