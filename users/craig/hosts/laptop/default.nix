{
  pkgs,
  disko,
  ...
}:

{
  imports = [
    ../../common.nix
    #../../vm-testing.nix
    ./hardware.nix
    (disko + "/laptop.nix")

    # Personal
    ../../flatpaks.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
  ];

  # Laptop power management
  services.power-profiles-daemon.enable = true;

  # Modules
  desktops.gnome.enable = true;
  services.tailscaleDesktop.enable = true;
  services.sshServer.enable = true;
  services.vpn.enable = true;
  services.flatpakBase.enable = true;
  security.yubikey.enable = true;
  development.enable = true;
  gaming.enable = true;
  autoUpgrade.enable = true;

  system.stateVersion = "25.11";
}
