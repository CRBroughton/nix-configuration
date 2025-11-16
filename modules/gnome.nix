{ pkgs, lib, config, ... }:

{
  # GNOME extensions are installed via the 'install-gnome-extensions' command
  # Run 'install-gnome-extensions' after home-manager switch to install:
  #   - Tiling Shell (v61)
  #   - Blur my Shell (v70)
  #   - Caffeine (v58)

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
        "blur-my-shell@aunetx"
        "caffeine@patapon.info"
      ];
    };
  };
}
