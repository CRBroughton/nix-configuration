{ config, pkgs, lib, ... }:

{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      accent-color = "purple";
    };
    "org/gnome/shell" = {
      disable-user-extensions = false;
      favorite-apps = [
        "app.zen_browser.zen.desktop"
        "org.gnome.Nautilus.desktop"
        "io.github.equicord.equibop.desktop"
        "com.bitwarden.desktop.desktop"
        "code.desktop"
        "com.mitchellh.ghostty.desktop"
        "steam.desktop"
        "net.lutris.Lutris.desktop"
        "com.heroicgameslauncher.hgl.desktop"
        "md.obsidian.Obsidian.desktop"
      ];
      enabled-extensions = [
        "blur-my-shell@aunetx"
        "caffeine@patapon.info"
        "appindicatorsupport@rgcjonas.gmail.com"
        "paperwm@paperwm.github.com"
      ];
    };
    "org/gnome/desktop/input-sources" = {
      sources = [
        (lib.hm.gvariant.mkTuple [ "xkb" "gb" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "us" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "colemak_dh_ansi" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "colemak_dh_iso" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "shavian" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "gallium_v2" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "graphite" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "canary" ])
      ];
    };
  };

  # GNOME Extensions
  home.packages = with pkgs; [
    gnomeExtensions.blur-my-shell
    gnomeExtensions.caffeine
    gnomeExtensions.appindicator
    gnomeExtensions.paperwm
  ];

  # Custom keyboard layouts
  home.file.".config/xkb/symbols".source = ../../xkb;
  home.file.".config/xkb/rules/evdev.xml".source = ../../xkb/evdev.xml;
}
