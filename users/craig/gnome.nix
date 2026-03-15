# Personal GNOME settings - theme, dock, extensions, keyboard layouts
{
  pkgs,
  lib,
  ...
}:

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
        "org.gajim.Gajim.desktop"
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
        "tilingshell@ferrarodomenico.com"
        "blur-my-shell@aunetx"
        "caffeine@patapon.info"
        "compiz-windows-effect@hermes83.github.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        #"paperwm@paperwm.github.com"
      ];
    };
    "org/gnome/desktop/app-folders" = {
      folder-children = [
        "System"
        "Utilities"
        "Media"
        "Office"
        "Games"
        "Social"
        "Dev"
      ];
    };
    "org/gnome/desktop/app-folders/folders/System" = {
      name = "System";
      apps = [
        "org.gnome.DiskUtility.desktop"
        "org.gnome.SystemMonitor.desktop"
        "org.gnome.Logs.desktop"
        "org.gnome.Software.desktop"
        "org.gnome.seahorse.Application.desktop"
        "org.gnome.Connections.desktop"
        "org.gnome.baobab.desktop"
        "org.gnome.Console.desktop"
        "org.gnome.Settings.desktop"
        "org.gnome.Extensions.desktop"
        "com.mattjakeman.ExtensionManager.desktop"
        "com.github.tchx84.Flatseal.desktop"
        "io.github.flattool.Warehouse.desktop"
        "it.mijorus.gearlever.desktop"
        "nixos-manual.desktop"
        "com.bitwarden.desktop.desktop"
        "btop.desktop"
        "org.kde.kleopatra.desktop"
        "org.rncbc.qpwgraph.desktop"
        "org.kde.kwatchgnupg.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Utilities" = {
      name = "Utilities";
      apps = [
        "org.gnome.Calculator.desktop"
        "org.gnome.Characters.desktop"
        "org.gnome.clocks.desktop"
        "org.gnome.font-viewer.desktop"
        "simple-scan.desktop"
        "yelp.desktop"
        "org.gnome.Epiphany.desktop"
        "com.brave.Browser.desktop"
        "com.transmissionbt.Transmission.desktop"
        "io.gitlab.news_flash.NewsFlash.desktop"
        "gscriptor.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Media" = {
      name = "Media";
      apps = [
        "org.gnome.Music.desktop"
        "org.gnome.Showtime.desktop"
        "org.gnome.Loupe.desktop"
        "org.gnome.Decibels.desktop"
        "org.gnome.Snapshot.desktop"
        "com.github.neithern.g4music.desktop"
        "org.musicbrainz.Picard.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Office" = {
      name = "Office";
      apps = [
        "org.gnome.Calendar.desktop"
        "org.gnome.Contacts.desktop"
        "org.gnome.Maps.desktop"
        "org.gnome.Weather.desktop"
        "org.gnome.TextEditor.desktop"
        "org.gnome.Papers.desktop"
        "md.obsidian.Obsidian.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Games" = {
      name = "Games";
      apps = [
        "steam.desktop"
        "net.lutris.Lutris.desktop"
        "com.heroicgameslauncher.hgl.desktop"
        "net.runelite.RuneLite.desktop"
        "Caves of Qud.desktop"
        "Dungeon Defenders.desktop"
        "Mewgenics.desktop"
        "Slay the Spire.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Social" = {
      name = "Social";
      apps = [
        "org.gajim.Gajim.desktop"
        "org.equicord.equibop.desktop"
        "info.mumble.Mumble.desktop"
      ];
    };
    "org/gnome/desktop/app-folders/folders/Dev" = {
      name = "Dev";
      apps = [
        "code.desktop"
        "dev.zed.Zed.desktop"
        "com.mitchellh.ghostty.desktop"
        "nvim.desktop"
        "virt-manager.desktop"
        "micro.desktop"
        "gvim.desktop"
        "vim.desktop"
        "xterm.desktop"
      ];
    };
    "org/gnome/desktop/input-sources" = {
      sources = [
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "gb"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "us"
        ])
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
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "canary"
        ])
      ];
    };
  };

  home.packages = with pkgs; [
    # Fonts
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    inter

    # GNOME Extensions
    gnomeExtensions.tiling-shell
    gnomeExtensions.blur-my-shell
    gnomeExtensions.caffeine
    gnomeExtensions.compiz-windows-effect
    gnomeExtensions.appindicator
    # gnomeExtensions.paperwm # trying this out in the future
  ];

  # Custom keyboard layouts
  home.file.".config/xkb/symbols".source = ../../xkb;
  home.file.".config/xkb/rules/evdev.xml".source = ../../xkb/evdev.xml;
}
