# Base flatpak configuration - apps everyone should have
{
  config,
  lib,
  ...
}:

let
  cfg = config.services.flatpakBase;
in
{
  options.services.flatpakBase = {
    enable = lib.mkEnableOption "Flatpak with Flathub remote and base desktop applications";
  };

  config = lib.mkIf cfg.enable {
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
  };
}
