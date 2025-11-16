{ ... }:

{
  # Flatpak applications - declaratively managed via nix-flatpak
  services.flatpak.enable = true;
  services.flatpak.packages = [
    "io.github.equicord.equibop"
    "app.zen_browser.zen"
    "com.google.Chrome"
    "com.bitwarden.desktop"
    "com.transmissionbt.Transmission"
    "com.heroicgameslauncher.hgl"
    "md.obsidian.Obsidian"
    "org.gnome.Loupe"
    "com.brave.Browser"
  ];
}
