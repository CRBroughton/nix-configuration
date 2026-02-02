{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Container and orchestration tools
    podman
    podman-compose
    lazydocker
    exercism

    # Version control
    lazygit
    jujutsu

    # JavaScript tools
    pnpm
    bun

    # PHP tools
    php
    php84Packages.composer
    laravel

    # API and HTTP testing
    posting
    hurl
    httpyac
    grpcurl

    # DevOps and automation
    ansible
    go-task
    just

    # Nix tools
    nil # Nix LSP

    # Data tools
    jq
    jnv
    protobuf

    # Build tools (needed for protobuf and telescope-fzf-native)
    gcc
    gnumake

    # Code analysis
    tokei
    claude-code
  ];
}
