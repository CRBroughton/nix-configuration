# Arion - Declarative Docker/Podman compose via Nix
{ config, lib, ... }:

let
  cfg = config.modules.server.arion;
in
{
  options.modules.server.arion = {
    enable = lib.mkEnableOption "Arion declarative container management with Docker backend";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };

    users.users.craig.extraGroups = [ "docker" ];

    virtualisation.arion = {
      backend = "docker";
    };

    systemd.tmpfiles.rules = [
      "d /var/lib/arion 0755 root root -"
    ];
  };
}
