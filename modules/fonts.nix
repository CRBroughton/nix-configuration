{ pkgs, ... }:

{
  # Font configuration
  fonts.fontconfig.enable = true;

  # Install fonts
  home.packages = with pkgs; [
    # Nerd Fonts
    nerd-fonts.fira-code
  ];
}