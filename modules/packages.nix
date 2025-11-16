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

    # File transfer and backup
    croc
    restic

    # GPG/YubiKey tools
    # kleopatra
    gnupg
    pcsc-tools
    yubikey-manager
    opensc

    # Terminal and audio
    # ghostty - using ghostty-wrapped module instead (see below)
    qpwgraph

    # Flatpak management
    flatpak

    # GNOME tools
    gnome-extension-manager

    # Additional utilities
    chezmoi  # For dotfiles management

    lutris
  ];
}
