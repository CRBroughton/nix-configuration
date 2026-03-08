# GNOME Desktop - GDM, GNOME, Pipewire, extensions, theme
{ config, pkgs, lib, user, ... }:

{
  # ============================================
  # NixOS (system level)
  # ============================================

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

  # Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    inter
    noto-fonts
    noto-fonts-color-emoji
  ];

  # ============================================
  # Home-manager (user level)
  # ============================================

  home-manager.users.${user} = { lib, pkgs, ... }: {
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
    home.packages = with pkgs.gnomeExtensions; [
      tiling-shell
      blur-my-shell
      caffeine
      compiz-windows-effect
      appindicator
      # paperwm // trying this out in the future
    ];

    # Custom keyboard layouts
    home.file.".config/xkb/symbols".source = ../../xkb;
    home.file.".config/xkb/rules/evdev.xml".source = ../../xkb/evdev.xml;
  };
}
