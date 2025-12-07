{ ... }:

{
  # Flatpak applications - declaratively managed via nix-flatpak
  services.flatpak.enable = true;
  services.flatpak.packages = [
    "com.mattjakeman.ExtensionManager"
    "io.github.equicord.equibop"
    "com.google.Chrome"
    "com.bitwarden.desktop"
    "com.transmissionbt.Transmission"
    "com.heroicgameslauncher.hgl"
    "md.obsidian.Obsidian"
    "org.gnome.Loupe"
    "com.brave.Browser"
    "com.github.neithern.g4music"
    "io.gitlab.news_flash.NewsFlash"
    "io.github.flattool.Warehouse"
    "com.github.tchx84.Flatseal"
  ];
}
