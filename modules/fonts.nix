{ pkgs, ... }:

{
  # Font configuration
  fonts.fontconfig.enable = true;

  # Install fonts
  home.packages = with pkgs; [
    # Nerd Fonts
    nerd-fonts.fira-code
  ];

  # Install Shavian fonts from local directory
  home.file.".local/share/fonts/shavian" = {
    source = ../fonts/shavian;
    recursive = true;
  };

  # Update font cache after installation
  home.activation.updateFontCache = {
    after = [ "writeBoundary" ];
    before = [ ];
    data = ''
      if command -v fc-cache &> /dev/null; then
        ${pkgs.fontconfig}/bin/fc-cache -fv ~/.local/share/fonts 2>/dev/null || true
      fi
    '';
  };
}
