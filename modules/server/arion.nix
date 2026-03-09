# Arion - Declarative Docker/Podman compose via Nix
{
  ...
}:

{
  # Use Docker as the backend (simpler than rootless podman for arion)
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };

  # Add user to docker group
  users.users.craig.extraGroups = [ "docker" ];

  # Arion backend configuration
  virtualisation.arion = {
    backend = "docker";
  };

  # Ensure data directories exist
  systemd.tmpfiles.rules = [
    "d /var/lib/arion 0755 root root -"
  ];
}
