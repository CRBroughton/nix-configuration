{ config, pkgs, lib, ... }:

{
  # XKB custom keyboard layouts
  home.file.".config/xkb/symbols/shavian".source = ../xkb/shavian;
  home.file.".config/xkb/symbols/gallium_v2".source = ../xkb/gallium_v2;
  home.file.".config/xkb/symbols/graphite".source = ../xkb/graphite;

  # dconf settings to register keyboard layouts in GNOME
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      sources = [
        # Default layouts
        (lib.hm.gvariant.mkTuple [ "xkb" "gb" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "us" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "us+colemak_dh" ])

        # Custom layouts
        (lib.hm.gvariant.mkTuple [ "xkb" "shavian" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "gallium_v2" ])
        (lib.hm.gvariant.mkTuple [ "xkb" "graphite" ])
      ];
      xkb-options = [ "terminate:ctrl_alt_bksp" ];
    };
  };
}
