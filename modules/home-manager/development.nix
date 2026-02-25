{ config, pkgs, ... }:

{
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
}
