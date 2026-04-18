# Base flatpak configuration - apps everyone should have
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.flatpak;

  notifyScript = pkgs.writeShellScript "flatpak-notify" ''
    msg_title="$1"
    msg_body="$2"
    uid=$(id -u ${user} 2>/dev/null) || exit 0
    bus="/run/user/$uid/bus"
    attempts=0
    while [ ! -S "$bus" ] && [ $attempts -lt 30 ]; do
      sleep 2
      attempts=$((attempts + 1))
    done
    if [ -S "$bus" ]; then
      ${pkgs.util-linux}/bin/runuser -u ${user} -- \
        env XDG_RUNTIME_DIR="/run/user/$uid" \
            DBUS_SESSION_BUS_ADDRESS="unix:path=$bus" \
        ${pkgs.libnotify}/bin/notify-send \
          --app-name="System" \
          --urgency=normal \
          "$msg_title" "$msg_body"
    fi
  '';
in
{
  options.modules.flatpak = {
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

    systemd.services.flatpak-notify-done = {
      description = "Notify user that flatpak apps are installed";
      wantedBy = [ "flatpak-managed-install.service" ];
      after = [ "flatpak-managed-install.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${notifyScript} 'Apps Ready' 'All applications have been installed.'";
        RemainAfterExit = false;
      };
    };
  };
}
