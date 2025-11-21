{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Shell and prompt
    fish
    starship
    zoxide
    bat

    # Text editors
    micro

    # Development tools
    ansible
    lazydocker
    lazygit
    podman
    pnpm
    bun
    btop
    navi
    jq
    jnv
    posting


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
    # ghostty - using ghostty-wrapped module instead
    qpwgraph

    # Flatpak management
    flatpak

    lutris
    warp
  ];
}
