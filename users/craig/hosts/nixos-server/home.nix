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

  # Enable podman socket for tools like lazydocker
  systemd.user.sockets.podman = {
    Install.WantedBy = [ "sockets.target" ];
  };

  # Docker compatibility
  home.sessionVariables = {
    DOCKER_HOST = "unix:///run/user/1000/podman/podman.sock";
  };
}
