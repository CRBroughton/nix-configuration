# Mum's GNOME settings - dock and theme
{ pkgs, lib, ... }:

{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
      sleep-inactive-battery-type = "nothing";
      idle-dim = false;
    };
    "org/gnome/desktop/session" = {
      idle-delay = lib.hm.gvariant.mkUint32 0;
    };
    "org/gnome/desktop/screensaver" = {
      lock-enabled = false;
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
    "org/gnome/desktop/remote-desktop/rdp" = {
      screen-share-mode = "mirror-primary";
      enable = true;
      view-only = false;
    };
  };

  home.packages = with pkgs; [
    gnomeExtensions.dash-to-dock
  ];
}
