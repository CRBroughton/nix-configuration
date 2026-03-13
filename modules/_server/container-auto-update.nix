# Container Auto-Update - Daily podman-compose pull and update
{ config, lib, pkgs, user, ... }:

let cfg = config.server.containerAutoUpdate; in
{
  options.server.containerAutoUpdate = {
    enable = lib.mkEnableOption "daily automatic podman container updates";
  };

  config = lib.mkIf cfg.enable {
    systemd.services."podman-auto-update" = {
      description = "Pull and update podman containers";
      path = [
        pkgs.podman-compose
        pkgs.podman
      ];
      serviceConfig = {
        Type = "oneshot";
        User = user;
        Group = "users";
        Environment = [
          "XDG_RUNTIME_DIR=/run/user/1000"
          "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus"
        ];
        ExecStart = pkgs.writeShellScript "podman-auto-update" ''
          for compose in /etc/nixos/services/*/compose.yaml; do
            dir=$(dirname "$compose")
            echo "Updating $dir..."
            cd "$dir"
            ${pkgs.podman-compose}/bin/podman-compose pull || true
            ${pkgs.podman-compose}/bin/podman-compose up -d
          done
          # Prune old images
          ${pkgs.podman}/bin/podman image prune -f
        '';
      };
    };

    systemd.timers."podman-auto-update" = {
      description = "Run podman auto-update daily";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "05:00";
        Persistent = true;
        RandomizedDelaySec = "30m";
      };
    };
  };
}
