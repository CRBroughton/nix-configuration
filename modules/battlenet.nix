# Battle.net (World of Warcraft retail / Classic) via Wine
#
# Usage:
#   1. battlenet-setup    - create Wine prefix, install DXVK + VKD3D, run Battle.net installer
#   2. battlenet          - launch Battle.net, log in, install WoW from launcher
#
# Tips:
#   - If Battle.net window is blank/black: Settings > App > disable
#     "Use browser hardware acceleration", restart.
#   - Retail WoW uses DX12 (VKD3D-Proton). If issues, switch Graphics API to
#     DirectX 11 in-game (DXVK).
{
  config,
  lib,
  pkgs,
  user,
  ...
}:
let
  cfg = config.modules.battlenet;

  # Battle.net needs recent Wine; staging tracks upstream fixes closely
  wine = pkgs.wineWow64Packages.staging;
  wineprefix = "$HOME/Games/battlenet";

  # Installer URL is unversioned (hash changes every release), so fetch at runtime
  installerUrl = "https://www.battle.net/download/getInstallerForGame?os=win&gameProgram=BATTLENET_APP&version=Live";

  env = ''
    export WINEPREFIX="${wineprefix}"
    export WINEARCH=win64
    export WINEESYNC=1
    export WINEFSYNC=1
    export WINEDEBUG=-all
    export DXVK_HUD=0
    export PATH="${wine}/bin:$PATH"
  '';

  setupScript = pkgs.writeShellScriptBin "battlenet-setup" ''
    set -e
    ${env}

    mkdir -p "$WINEPREFIX"

    echo "Setting up Wine prefix..."
    wineboot -i

    echo "Setting Windows version to 10..."
    ${pkgs.winetricks}/bin/winetricks -q win10

    echo "Installing DXVK (DX11) and VKD3D-Proton (DX12)..."
    ${pkgs.winetricks}/bin/winetricks -q dxvk vkd3d

    echo "Installing core fonts (fixes Battle.net login UI text)..."
    ${pkgs.winetricks}/bin/winetricks -q corefonts

    echo "Downloading Battle.net installer..."
    tmp="$(mktemp -d)"
    trap 'rm -rf "$tmp"' EXIT
    ${pkgs.curl}/bin/curl -fL -o "$tmp/Battle.net-Setup.exe" "${installerUrl}"

    echo "Running Battle.net installer..."
    wine "$tmp/Battle.net-Setup.exe"
    wineserver -w

    echo "Done! Run 'battlenet' to launch, then install WoW from the launcher."
  '';

  playScript = pkgs.writeShellScriptBin "battlenet" ''
    ${env}

    launcher="$WINEPREFIX/drive_c/Program Files (x86)/Battle.net/Battle.net Launcher.exe"
    if [ ! -f "$launcher" ]; then
      echo "Battle.net not found, run 'battlenet-setup' first!"
      exit 1
    fi

    ulimit -n 524288 2>/dev/null || true

    exec ${pkgs.gamemode}/bin/gamemoderun wine "$launcher"
  '';
in
{
  options.modules.battlenet = {
    enable = lib.mkEnableOption "Battle.net (World of Warcraft) via Wine";
  };

  config = lib.mkIf cfg.enable {
    # DXVK/VKD3D need Vulkan + 32-bit GL for the launcher
    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = true;

    home-manager.users.${user} = {
      home.packages = [
        setupScript
        playScript
      ];

      xdg.desktopEntries.battlenet = {
        name = "Battle.net";
        exec = "battlenet";
        comment = "Battle.net launcher (World of Warcraft)";
        categories = [ "Game" ];
      };
    };
  };
}
