# Home-manager config for nixos-server (minimal, headless)
{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../../../modules/shell.nix
    ../../../../modules/git.nix
    ../../git.nix   # Personal git config
  ];

  home.username = "craig";
  home.homeDirectory = "/home/craig";
  home.stateVersion = "25.11";

  # Podman configuration for rootless containers
  home.file.".config/containers/registries.conf".text = ''
    unqualified-search-registries = ["docker.io", "ghcr.io", "quay.io"]
  '';

  home.file.".config/containers/policy.json".text = builtins.toJSON {
    default = [{ type = "insecureAcceptAnything"; }];
  };

  # Podman socket for tools like lazydocker
  systemd.user.services.podman-socket-restart = {
    Unit = {
      Description = "Restart podman socket on boot";
      After = [ "default.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/systemctl --user restart podman.socket";
      RemainAfterExit = true;
    };
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.services.podman = {
    Service = {
      Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
    };
  };

  # Docker compatibility
  home.sessionVariables = {
    DOCKER_HOST = "unix:///run/user/1000/podman/podman.sock";
  };
}
