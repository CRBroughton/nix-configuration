# Development - Virtualisation, containers, languages, and tools
{ config, pkgs, user, ... }:

{
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
      nil  # Nix LSP

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

      # Other editors
      micro
    ];
  };
}
