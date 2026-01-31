{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # GPG/YubiKey tools
    kdePackages.kleopatra
    gnupg
    pcsc-tools
    yubikey-manager
    yubico-pam
    opensc
  ];
}
