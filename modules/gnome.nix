{ pkgs, lib, config, ... }:

{
  # GNOME extensions are installed via the 'install-gnome-extensions' command
  # Run 'install-gnome-extensions' after home-manager switch to install:
  #   - Tiling Shell (v61)
  #   - Blur my Shell (v70)
  #   - Caffeine (v58)

  # Refresh GNOME app cache after home-manager switch
  home.activation.refreshGnomeApps = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if command -v update-desktop-database &> /dev/null; then
      ${pkgs.desktop-file-utils}/bin/update-desktop-database ~/.local/share/applications 2>/dev/null || true
    fi
  '';

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
        "ghostty.desktop"
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
