# Albion Online - native Linux client via FHS environment
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.albionOnline;

  version = "20260504100546";

  src = pkgs.fetchurl {
    url = "https://live.albiononline.com/clients/${version}/albion-online-setup";
    hash = "sha256-ynI9RglRofEDbNQttBdsxBqMGDzHSaTQTZf5KiYDCps=";
  };

  gameDeps =
    pkgs: with pkgs; [
      alsa-lib
      curl
      dbus
      expat
      fontconfig
      freetype
      glib
      krb5
      libGL
      libpulseaudio
      libuuid
      libX11
      libXau
      libXcomposite
      libXcursor
      libXdamage
      libXdmcp
      libXext
      libXfixes
      libXi
      libXinerama
      libXrandr
      libXrender
      libXScrnSaver
      libXtst
      libXxf86vm
      mesa
      nspr
      nss
      openal
      stdenv.cc.cc.lib
      udev
      zlib
    ];

  albion-setup-bin = pkgs.runCommand "albion-online-setup" { } ''
    install -m755 ${src} $out
  '';

  albion-installer = pkgs.buildFHSEnv {
    name = "albion-online-installer";
    targetPkgs = gameDeps;
    runScript = albion-setup-bin;
  };

  albion-online = pkgs.buildFHSEnv {
    name = "albion-online";
    targetPkgs = gameDeps;
    runScript = pkgs.writeShellScript "albion-launch" ''
      INSTALL_DIR="''${ALBION_INSTALL_DIR:-$HOME/albiononline}"
      LAUNCHER="$INSTALL_DIR/launcher/Albion-Online"
      if [ ! -f "$LAUNCHER" ]; then
        echo "Albion Online not found at $INSTALL_DIR"
        echo "Run 'albion-online-installer' to install first, or set ALBION_INSTALL_DIR."
        exit 1
      fi
      exec "$LAUNCHER" "$@"
    '';
  };
in
{
  options.modules.albionOnline = {
    enable = lib.mkEnableOption "Albion Online";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = [
        albion-installer
        albion-online
      ];

      xdg.desktopEntries.albion-online = {
        name = "Albion Online";
        exec = "albion-online";
        icon = "albion-online";
        comment = "Play Albion Online";
        categories = [ "Game" ];
      };
    };
  };
}
