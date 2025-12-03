{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Shell and CLI utilities
    fish
    starship
    zoxide
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

    # JavaScript tools
    pnpm
    bun

    # PHP tools
    php
    php84Packages.composer

    # API and HTTP testing
    posting
    hurl
    httpyac
    grpcurl

    # DevOps and automation
    ansible
    go-task

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

    # Gaming
    lutris

    # Terminal
    # ghostty - using ghostty-wrapped module instead
    warp
  ];
}
