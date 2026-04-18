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
    version = "v0.1-10998";
    src = pkgs.fetchurl {
      url = "https://github.com/stenzek/duckstation/releases/download/v0.1-10998/DuckStation-x64.AppImage";
      hash = "sha256-sgSIa7SY7eGikCFfwu+1IcDC8muWR4jfaXtPwss/f3s=";
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
        mgba
        duckstation
      ];
    };
  };
}
