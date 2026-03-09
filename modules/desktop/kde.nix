# KDE Plasma desktop environment
{
  pkgs,
  ...
}:

{
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.xserver.xkb.layout = "gb,us";

  # Exclude some KDE packages
  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    elisa # Music player
    konsole # Use Ghostty instead
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
