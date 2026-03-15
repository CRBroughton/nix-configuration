# Podman - Rootless container runtime
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.server.podman;
in
{
  options.modules.server.podman = {
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

    # User-level podman socket and registries (via podman-flake)
    home-manager.users.${user} = {
      programs.podman-config = {
        enable = true;
        registries = [
          "docker.io"
          "ghcr.io"
          "quay.io"
        ];
      };

      # Ensure podman socket is enabled and started on boot
      systemd.user.services.podman-socket-enable = {
        Unit = {
          Description = "Enable and start podman socket on boot";
          After = [ "default.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.systemd}/bin/systemctl --user enable --now podman.socket";
          RemainAfterExit = true;
        };
        Install.WantedBy = [ "default.target" ];
      };

      # Add PATH for rootless podman (newuidmap/newgidmap wrappers)
      systemd.user.services.podman = {
        Service = {
          Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
        };
      };
    };
  };
}
