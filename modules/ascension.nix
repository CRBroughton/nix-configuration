# Ascension WoW launcher via Wine
{
  config,
  lib,
  pkgs,
  user,
  ...
}:
let
  cfg = config.modules.ascension;

  ascensionInstaller = pkgs.fetchurl {
    url = "https://cdn-launcher.ascension-patch.com/update/ascension-setup-1.0.97.exe";
    sha256 = "sha256-oYuAaBccoqCYmeYiRXyewFXlabvfaavEWbP/4sz7PzU=";
  };

  wine = pkgs.wineWow64Packages.stable;
  wineprefix = "$HOME/Games/ascension-wow";

  setupScript = pkgs.writeShellScriptBin "ascension-setup" ''
    export WINEPREFIX="${wineprefix}"
    export WINEESYNC=1
    export WINEFSYNC=1
    export WINEDEBUG=-all

    mkdir -p "$WINEPREFIX"

    echo "Setting up Wine prefix..."
    ${wine}/bin/wineboot -i

    echo "Setting Windows version to 10..."
    ${pkgs.winetricks}/bin/winetricks -q win10

    echo "Installing vcrun2015..."
    ${pkgs.winetricks}/bin/winetricks -q vcrun2015

    echo "Installing DXVK..."
    ${pkgs.winetricks}/bin/winetricks -q dxvk

    echo "Disabling divxtac.dll..."
    ${wine}/bin/wine reg add \
      "HKEY_CURRENT_USER\Software\Wine\DllOverrides" \
      /v divxtac /t REG_SZ /d "" /f

    echo "Wine prefix ready! Now run 'ascension-install'."
  '';

  installScript = pkgs.writeShellScriptBin "ascension-install" ''
    export WINEPREFIX="${wineprefix}"
    export WINEESYNC=1
    export WINEFSYNC=1
    export WINEDEBUG=-all

    if [ ! -d "$WINEPREFIX" ]; then
      echo "Wine prefix not found, run 'ascension-setup' first!"
      exit 1
    fi

    echo "Running Ascension installer..."
    ${wine}/bin/wine ${ascensionInstaller}

    echo "Installer done! Use the launcher to download the game files."
  '';

  playScript = pkgs.writeShellScriptBin "ascension" ''
    export WINEPREFIX="${wineprefix}"
    export WINEESYNC=1
    export WINEFSYNC=1
    export WINEDEBUG=-all

    ulimit -n 524288

    ${wine}/bin/wine \
      "${wineprefix}/drive_c/Program Files/Ascension Launcher/Ascension Launcher.exe"
  '';

in
{
  options.modules.ascension = {
    enable = lib.mkEnableOption "Ascension WoW launcher via Wine";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = [
        setupScript
        installScript
        playScript
      ];
    };
  };
}
