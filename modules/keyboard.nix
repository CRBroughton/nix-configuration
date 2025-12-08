{
  config,
  pkgs,
  lib,
  ...
}:

{
  # XKB custom keyboard layouts
  home.file.".config/xkb/symbols/colemak_dh_ansi".source = ../xkb/colemak_dh_ansi;
  home.file.".config/xkb/symbols/colemak_dh_iso".source = ../xkb/colemak_dh_iso;
  home.file.".config/xkb/symbols/canary".source = ../xkb/canary;
  home.file.".config/xkb/symbols/shavian".source = ../xkb/shavian;
  home.file.".config/xkb/symbols/gallium_v2".source = ../xkb/gallium_v2;
  home.file.".config/xkb/symbols/graphite".source = ../xkb/graphite;

  # XKB rules file to register custom layouts with GNOME
  home.file.".config/xkb/rules/evdev.xml".source = ../xkb/evdev.xml;

  # dconf settings to register keyboard layouts in GNOME
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      sources = [
        # Default layouts
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "gb"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "us"
        ])

        # Custom layouts
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "colemak_dh_ansi"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "colemak_dh_iso"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "canary"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "shavian"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "gallium_v2"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "graphite"
        ])
      ];
      xkb-options = [ "terminate:ctrl_alt_bksp" ];
    };
  };
}
