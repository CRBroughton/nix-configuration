# Restic - Backup to Backblaze B2
# Requires /etc/restic-env-password and /etc/restic-env files
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.server.restic;
in
{
  options.server.restic = {
    enable = lib.mkEnableOption "Restic backups to Backblaze B2";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.restic ];

    services.restic.backups = {
      b2 = {
        repository = "s3:https://s3.eu-central-003.backblazeb2.com/crbroughton-nixos-server";
        passwordFile = "/etc/restic-env-password";
        environmentFile = "/etc/restic-env";
        paths = [
          "/etc/nixos/services"
          "/var/lib/containers/storage/volumes"
        ];
        exclude = [
          "/etc/nixos/services/*/tailscale"
          "/var/lib/containers/storage/volumes/ollama-data"
        ];
        timerConfig = {
          OnCalendar = "02:00";
          Persistent = true;
        };
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 4"
          "--keep-monthly 6"
        ];
      };
    };
  };
}
