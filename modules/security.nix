# Security - Polkit, Yubikey support and tools
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.security.yubikey;
in
{
  options.security.yubikey = {
    enable = lib.mkEnableOption "Polkit and Yubikey support with smartcard daemon and management tools";
  };

  config = lib.mkIf cfg.enable {
    security.polkit.enable = true;

    # Yubikey support
    services.pcscd.enable = true;
    services.udev.packages = [ pkgs.yubikey-personalization ];

    home-manager.users.${user} = {
      home.packages = with pkgs; [
        kdePackages.kleopatra
        gnupg
        pcsc-tools
        yubikey-manager
      ];
    };
  };
}
