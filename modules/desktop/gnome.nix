{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.gnome;
in
{
  options.modules.gnome = {
    enable = lib.mkEnableOption "GNOME Desktop Environment";

    apps = {
      baseApplications = lib.mkEnableOption "base GNOME apps" // {
        default = true;
      };
      extraApplications = lib.mkEnableOption "extra GNOME apps";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.enable = true;
    services.displayManager.gdm.enable = true;
    services.desktopManager.gnome.enable = true;
    services.xserver.xkb.layout = "gb,us";

    # Manage apps manually via apps.base / apps.extras
    services.gnome.core-apps.enable = false;

    # Exclude gnome-tour (part of core-shell, not core-apps)
    environment.gnome.excludePackages = with pkgs; [
      gnome-tour
    ];

    # Base apps — enabled by default
    # (Settings/gnome-control-center is always present via core-shell)
    environment.systemPackages =
      lib.optionals cfg.apps.baseApplications (
        with pkgs;
        [
          nautilus # Files
          gnome-weather # Weather
          gnome-system-monitor # System Monitor
          gnome-logs # Logs
          gnome-calendar # Calendar
          gnome-software # Software
          gnome-calculator
        ]
      )
      ++ lib.optionals cfg.apps.extraApplications (
        with pkgs;
        [
          baobab # Disk Usage Analyzer
          decibels # Audio player
          epiphany # Web browser
          gnome-text-editor
          gnome-characters
          gnome-clocks
          gnome-console
          gnome-contacts
          gnome-font-viewer
          gnome-maps
          gnome-music
          loupe # Image viewer
          papers # Document viewer
          gnome-connections
          showtime # Video player
          simple-scan
          snapshot # Camera
          yelp # Help
        ]
      );

    programs.gnome-disks.enable = lib.mkIf cfg.apps.baseApplications true; # Disks
    programs.seahorse.enable = lib.mkIf cfg.apps.baseApplications true; # Passwords & Keys
    services.gnome.sushi.enable = lib.mkIf cfg.apps.baseApplications true; # File previewer

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
  };
}
