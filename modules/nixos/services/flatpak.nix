{ config, pkgs, ... }:

{
  services.flatpak = {
    enable = true;
    remotes = [
      { name = "flathub"; location = "https://dl.flathub.org/repo/flathub.flatpakrepo"; }
    ];
    packages = [
      "app.zen_browser.zen"
      "com.google.Chrome"
      "com.brave.Browser"
      "com.bitwarden.desktop"
      "md.obsidian.Obsidian"
      "com.heroicgameslauncher.hgl"
      "io.github.equicord.equibop"
      "com.transmissionbt.Transmission"
      "com.mattjakeman.ExtensionManager"
      "io.github.flattool.Warehouse"
      "com.github.tchx84.Flatseal"
    ];
  };
}
