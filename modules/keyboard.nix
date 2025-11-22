{ config, pkgs, lib, ... }:

{
  # XKB custom keyboard layouts
  home.file.".config/xkb/symbols/shavian".source = ../xkb/shavian;
  home.file.".config/xkb/symbols/gallium_v2".source = ../xkb/gallium_v2;
  home.file.".config/xkb/symbols/graphite".source = ../xkb/graphite;

  # XKB rules file to register custom layouts with GNOME
  home.file.".config/xkb/rules/evdev.xml".text = ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE xkbConfigRegistry SYSTEM "xkb.dtd">
    <xkbConfigRegistry version="1.1">
      <layoutList>
        <layout>
          <configItem>
            <name>shavian</name>
            <shortDescription>Shavian</shortDescription>
            <description>English (Shavian)</description>
            <languageList>
              <iso639Id>eng</iso639Id>
            </languageList>
          </configItem>
          <variantList>
            <variant>
              <configItem>
                <name>iykury</name>
                <description>English (Shavian, Iykury)</description>
              </configItem>
            </variant>
            <variant>
              <configItem>
                <name>igc</name>
                <description>English (Shavian, Imperial)</description>
              </configItem>
            </variant>
            <variant>
              <configItem>
                <name>qwerty</name>
                <description>English (Shavian, QWERTY)</description>
              </configItem>
            </variant>
            <variant>
              <configItem>
                <name>qwerty_inverted</name>
                <description>English (QWERTY, Shavian on AltGr)</description>
              </configItem>
            </variant>
          </variantList>
        </layout>
        <layout>
          <configItem>
            <name>gallium_v2</name>
            <shortDescription>Gallium</shortDescription>
            <description>English (Gallium)</description>
            <languageList>
              <iso639Id>eng</iso639Id>
            </languageList>
          </configItem>
        </layout>
        <layout>
          <configItem>
            <name>graphite</name>
            <shortDescription>Graphite</shortDescription>
            <description>English (Graphite)</description>
            <languageList>
              <iso639Id>eng</iso639Id>
            </languageList>
          </configItem>
        </layout>
      </layoutList>
    </xkbConfigRegistry>
  '';

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
