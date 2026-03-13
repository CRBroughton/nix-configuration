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
    (disko + "/gaming-pc.nix")

    # Personal
    ../../flatpaks.nix
  ];

  # Kernel - CachyOS with sched_ext (uncomment for AMD gaming PC)
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
  ];

  # Modules
  desktops.gnome.enable = true;
  services.tailscaleDesktop.enable = true;
  services.sshServer.enable = true;
  services.vpn.enable = true;
  services.flatpakBase.enable = true;
  security.yubikey.enable = true;
  development.enable = true;
  gaming.enable = true;

  system.stateVersion = "25.11";
}
