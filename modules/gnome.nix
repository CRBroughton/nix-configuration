{
  pkgs,
  lib,
  config,
  ...
}:

{
  home.packages = with pkgs.gnomeExtensions; [
    tiling-shell
    blur-my-shell
    caffeine
    compiz-windows-effect
    appindicator
    paperwm
  ];

  # Link GNOME extensions so GNOME Shell can find them
  home.activation.linkGnomeExtensions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    EXTENSIONS_DIR="${config.home.homeDirectory}/.local/share/gnome-shell/extensions"
    mkdir -p "$EXTENSIONS_DIR"

    # Link each extension from Nix profile to local directory
    for ext in ${config.home.profileDirectory}/share/gnome-shell/extensions/*; do
      if [ -d "$ext" ]; then
        ext_name=$(basename "$ext")
        $DRY_RUN_CMD ln -sfn "$ext" "$EXTENSIONS_DIR/$ext_name"
      fi
    done
  '';

  # Refresh GNOME app cache after home-manager switch
  home.activation.refreshGnomeApps = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if command -v update-desktop-database &> /dev/null; then
      ${pkgs.desktop-file-utils}/bin/update-desktop-database ~/.local/share/applications 2>/dev/null || true
    fi
  '';

  # GNOME dconf settings
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      accent-color = "purple";
    };
    # Set favorite apps in GNOME dock
    "org/gnome/shell" = {
      disable-user-extensions = false;
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
      # Enable all installed extensions
      enabled-extensions = [
        "tilingshell@ferrarodomenico.com"
        "blur-my-shell@aunetx"
        "caffeine@patapon.info"
        "compiz-windows-effect@hermes83.github.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "paperwm@paperwm.github.com"
      ];
    };
  };
}
