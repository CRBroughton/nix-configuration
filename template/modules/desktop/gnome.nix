# GNOME Desktop - enable with: desktops.gnome.enable = true
{ config, lib, ... }:

let
  cfg = config.desktops.gnome;
in
{
  options.desktops.gnome = {
    enable = lib.mkEnableOption "GNOME desktop environment with GDM and Pipewire";
  };

  config = lib.mkIf cfg.enable {
    services.xserver.enable = true;
    services.displayManager.gdm.enable = true;
    services.desktopManager.gnome.enable = true;
    services.xserver.xkb.layout = "gb,us"; # Change to match your layout

    # Audio (Pipewire)
    services.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
