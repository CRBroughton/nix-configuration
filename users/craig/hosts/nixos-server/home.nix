# Home-manager config for nixos-server (minimal, headless)
{
  pkgs,
  ...
}:

{
  imports = [
    ../../../../modules/_home/shell.nix
    ../../../../modules/_home/git.nix
    ../../git.nix # Personal git config
  ];

  home.username = "craig";
  home.homeDirectory = "/home/craig";
  home.stateVersion = "25.11";

  # Podman with user socket for lazydocker/glance (via podman-flake)
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
}
