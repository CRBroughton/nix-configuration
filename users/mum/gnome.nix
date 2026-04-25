# Mum's GNOME settings - dock and theme
{ pkgs, ... }:

{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
    "org/gnome/shell" = {
      disable-user-extensions = false;
      favorite-apps = [
        "app.zen_browser.zen.desktop"
        "org.gnome.Nautilus.desktop"
        "com.bitwarden.desktop.desktop"
        "org.gajim.Gajim.desktop"
        "steam.desktop"
        "com.obsproject.Studio.desktop"
      ];
      enabled-extensions = [
        "dash-to-dock@micxgx.gmail.com"
      ];
    };
    "org/gnome/shell/extensions/dash-to-dock" = {
      dock-position = "LEFT";
    };
    "org/gnome/desktop/notifications/application/org-gnome-nautilus" = {
      enable = false;
    };
  };

  home.packages = with pkgs; [
    gnomeExtensions.dash-to-dock
  ];
}
