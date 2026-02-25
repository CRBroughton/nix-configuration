{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    kdePackages.kleopatra
    gnupg
    pcsc-tools
    yubikey-manager
  ];
}
