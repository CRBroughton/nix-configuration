{ pkgs, ... }:

{
  # Ghostty terminal with nixGL wrapper (for non-NixOS systems)
  programs.ghostty-wrapped = {
    enable = true;
    glWrapper = "nixGLIntel";  # Change to nixGLNvidia if you have NVIDIA GPU
    settings = {
      # Shell
      shell-integration = "fish";
      command = "${pkgs.fish}/bin/fish";

      # Font
      font-family = "FiraCode Nerd Font Mono";
      font-size = 14;

      # Colors
      background = "#171717";
      foreground = "#dedacf";
    };
  };
}
