# Podman - Rootless container runtime for servers
{ config, lib, pkgs, ... }:

let cfg = config.server.podman; in
{
  options.server.podman = {
    enable = lib.mkEnableOption "Podman rootless container runtime";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      enable = true;
      dockerCompat = false; # Disabled - arion uses real docker
      defaultNetwork.settings.dns_enabled = true;
    };

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

    environment.systemPackages = with pkgs; [
      podman-compose
      lazydocker
    ];
  };
}
