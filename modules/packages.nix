{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Shell and CLI utilities
    bat
    eza
    navi

    # Text editors
    micro

    # System monitoring
    btop

    # Container and orchestration tools
    podman
    lazydocker

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

    # Data tools
    jq
    jnv
    gcc # needed for protobuf compilation
    protobuf

    # Code analysis
    tokei

    # File transfer and backup
    croc
    restic

    # GPG/YubiKey tools
    kdePackages.kleopatra
    gnupg
    pcsc-tools
    yubikey-manager
    yubico-pam
    opensc

    # Audio
    qpwgraph

    # Package management
    flatpak
    nix-search-cli

    # Gaming
    lutris

    # Terminal
    # ghostty - using ghostty-wrapped module instead
    warp

    # __DYNAMIC_CLI_PACKAGES__ - Packages added via 'nix-just add' command
    # DO NOT REMOVE THIS MARKER - Used by automation scripts
  ];
}
