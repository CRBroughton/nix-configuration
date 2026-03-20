# Gaming Emulation
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.gamingEmulation;

  duckstation = pkgs.appimageTools.wrapType2 {
    pname = "duckstation";
    version = "latest";
    src = pkgs.fetchurl {
      url = "https://github.com/stenzek/duckstation/releases/download/latest/DuckStation-x64.AppImage";
      hash = "sha256-6LkTXBZYwEloCT+kcgJCB351NpU0W/LXo0NO+5yKw/E=";
    };
  };
in
{
  options.modules.gamingEmulation = {
    enable = lib.mkEnableOption "gaming emulators";
  };

  config = lib.mkIf cfg.enable {
    # ============================================
    # Home-manager (user level)
    # ============================================

    home-manager.users.${user} = {
      home.packages = with pkgs; [
        snes9x
        sameboy
        pcsx2
        duckstation
      ];
    };
  };
}
