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

      # Add your Ghostty configuration here
      # Example:
      # theme = "catppuccin-mocha";
      # font-family = "JetBrainsMono Nerd Font";
    };
  };
}
