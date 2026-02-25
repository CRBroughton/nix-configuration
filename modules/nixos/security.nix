{ config, pkgs, ... }:

{
  security.polkit.enable = true;

  # Yubikey support
  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];
}
