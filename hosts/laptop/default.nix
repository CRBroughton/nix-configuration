{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ../../disko/laptop.nix
    ../../profiles/workstation.nix
  ];

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel - Use Zen for VM testing, CachyOS for real hardware
  # Uncomment cachyos for real hardware:
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  boot.kernelPackages = pkgs.linuxPackages_zen;

  # User
  users.users.craig = {
    isNormalUser = true;
    description = "Craig";
    extraGroups = [ "wheel" "networkmanager" "libvirtd" "audio" "video" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk"
    ];
  };

  # VM testing settings - remove for real hardware
  users.users.craig.initialPassword = "test";
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "craig";

  system.stateVersion = "25.11";

  # VM testing settings (only affects nixos-rebuild build-vm)
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
