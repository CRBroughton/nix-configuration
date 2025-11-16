{ ... }:

{
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
    };
  };
}
