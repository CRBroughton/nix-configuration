# Security - Polkit, Yubikey support and tools
{ config, pkgs, user, ... }:

{
  # ============================================
  # NixOS (system level)
  # ============================================

  security.polkit.enable = true;

  # Yubikey support
  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  # ============================================
  # Home-manager (user level)
  # ============================================

  home-manager.users.${user} = {
    home.packages = with pkgs; [
      kdePackages.kleopatra
      gnupg
      pcsc-tools
      yubikey-manager
    ];
  };
}
