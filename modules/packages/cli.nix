{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Shell and CLI utilities
    bat
    eza
    navi
    tor-browser

    # Text editors
    micro
    emacs

    # System monitoring
    btop

    # File transfer and backup
    croc
    restic
    pika-backup

    # Terminal
    # ghostty - using ghostty-wrapped module instead
    warp
  ];
}
