# Pi Monitor - Raspberry Pi 3 B+ running Uptime Kuma
{ config, pkgs, lib, modulesPath, modules, ... }:

{
  imports = [
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  # Pi 3 B+ specific settings
  hardware.enableRedistributableFirmware = true;

  # Needed for Pi 3
  boot.kernelParams = [ "console=ttyS1,115200n8" ];

  # Nix settings
  nix.settings = {
    max-jobs = 2;  # Pi 3 memory constraints (1GB RAM)
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "root" "craig" ];
    require-sigs = false;  # Allow unsigned builds from server
  };

  # Swap helps on 1GB Pi
  swapDevices = [{
    device = "/swapfile";
    size = 1024;  # 1GB swap
  }];

  networking.hostName = "pi-monitor";
  networking.networkmanager.enable = true;

  # Enable SSH
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  # User configuration
  users.users.craig = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk"
    ];
  };
  security.sudo.wheelNeedsPassword = false;

  # Tailscale for VPN access
  services.tailscale.enable = true;

  # Minimal packages (Pi 3 has limited space/RAM)
  environment.systemPackages = with pkgs; [
    vim
    git
    btop
    curl
  ];

  # Use OCI containers (lighter than full podman daemon)
  virtualisation.oci-containers = {
    backend = "podman";
    containers.uptime-kuma = {
      image = "louislam/uptime-kuma:2";
      volumes = [
        "uptime-kuma:/app/data"
      ];
      autoStart = true;
      extraOptions = [ "--network=host" ];  # Use host network for Tailscale access
    };
  };

  # Firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 3001 ];  # SSH + Uptime Kuma
    trustedInterfaces = [ "tailscale0" ];
  };

  system.stateVersion = "25.11";
}
