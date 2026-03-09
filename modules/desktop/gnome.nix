# GNOME Desktop - Base system setup (GDM, GNOME, Pipewire, fonts)
{
  pkgs,
  ...
}:

{
  services.xserver.enable = true;
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
  services.xserver.xkb.layout = "gb,us";

  # Exclude some GNOME packages
  environment.gnome.excludePackages = with pkgs; [
    gnome-tour
    gnome-music
    epiphany
  ];

  # Audio (Pipewire)
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Base fonts (emoji support)
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-color-emoji
  ];
}
