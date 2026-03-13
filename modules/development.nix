# Development - Virtualisation, containers, languages, and tools
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.development;
in
{
  options.development = {
    enable = lib.mkEnableOption "development environment with Podman, libvirt, and common languages and tools";
  };

  config = lib.mkIf cfg.enable {
    # ============================================
    # NixOS (system level)
    # ============================================

    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
    };

    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;

    # ============================================
    # Home-manager (user level)
    # ============================================

    home-manager.users.${user} = {
      home.packages = with pkgs; [
        # Languages
        go
        zig
        zls
        nodejs
        pnpm
        bun
        rustup
        php
        php84Packages.composer

        # Development tools
        lazydocker
        podman-compose
        ansible
        go-task
        gcc
        gnumake
        protobuf
        hurl
        grpcurl
        claude-code
      ];
    };
  };
}
