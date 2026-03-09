# Base flatpak configuration - apps everyone should have
_:

{
  services.flatpak = {
    enable = true;
    remotes = [
      {
        name = "flathub";
        location = "https://dl.flathub.org/repo/flathub.flatpakrepo";
      }
    ];
    packages = [
      "app.zen_browser.zen"
      "com.bitwarden.desktop"
      "com.mattjakeman.ExtensionManager"
      "com.github.tchx84.Flatseal"
      "org.gajim.Gajim"
    ];
  };
}
