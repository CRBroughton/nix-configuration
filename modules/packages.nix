{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Shell and prompt
    fish
    starship

    # Text editors
    micro

    # Development tools
    ansible
    lazydocker
    lazygit
    podman
    pnpm
    bun

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

    # Terminal and audio
    # ghostty - using ghostty-wrapped module instead (see below)
    qpwgraph

    # Flatpak management
    flatpak

    # Additional utilities
    chezmoi  # For dotfiles management

    lutris
    warp
  ];
}
