{ pkgs, lib, ... }:

let
  # Override tiling-shell to use version 17.1 for GNOME Shell 49
  tiling-shell-v17 = pkgs.gnomeExtensions.tiling-shell.overrideAttrs (oldAttrs: rec {
    version = "17.1";
    src = pkgs.fetchFromGitHub {
      owner = "domferr";
      repo = "tilingshell";
      rev = version;
      hash = "sha256-0000000000000000000000000000000000000000000="; # Placeholder - will be replaced by correct hash
    };
  });
in
{
  # GNOME Extensions
  home.packages = with pkgs; [
    # Use our custom version of tiling-shell
    tiling-shell-v17

    # Environment/integration packages
    # glib-networking
    # gsettings-desktop-schemas
    # cacert  # CA certificates for TLS/SSL
  ];

  # Environment variables for GNOME apps to find TLS support
  # home.sessionVariables = {
  #   GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules";
  #   GST_PLUGIN_SYSTEM_PATH_1_0 = "${pkgs.glib-networking}/lib/gstreamer-1.0";
  # };

  # GNOME dconf settings
  dconf.settings = {
    # Set favorite apps in GNOME dock
    "org/gnome/shell" = {
      favorite-apps = [
        "app.zen_browser.zen.desktop"
        "org.gnome.Nautilus.desktop"
        "io.github.equicord.equibop.desktop"
        "com.bitwarden.desktop.desktop"
        "code.desktop"
        "com.mitchellh.ghostty.desktop"
        "com.ranfdev.DistroShelf.desktop"
        "steam.desktop"
        "net.lutris.Lutris.desktop"
        "com.heroicgameslauncher.hgl.desktop"
        "md.obsidian.Obsidian.desktop"
      ];
      enabled-extensions = [
        "tilingshell@ferrarodomenico.com"
      ];
    };
  };
}
