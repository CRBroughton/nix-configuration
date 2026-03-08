# Podman - Rootless container runtime for servers
{ config, pkgs, ... }:

{
  # Enable podman
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;  # Create docker alias
    defaultNetwork.settings.dns_enabled = true;
  };

  # UID/GID mapping for rootless containers
  security.wrappers = {
    newuidmap = {
      source = "${pkgs.shadow.out}/bin/newuidmap";
      setuid = true;
      owner = "root";
      group = "root";
    };
    newgidmap = {
      source = "${pkgs.shadow.out}/bin/newgidmap";
      setuid = true;
      owner = "root";
      group = "root";
    };
  };

  # Podman packages
  environment.systemPackages = with pkgs; [
    podman-compose
    lazydocker
  ];
}
