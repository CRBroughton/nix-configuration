{ config, pkgs, ... }:

{
  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # User configuration
  users.users.craig = {
    isNormalUser = true;
    description = "Craig";
    extraGroups = [ "wheel" "networkmanager" "libvirtd" "audio" "video" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk"
    ];
  };

  # VM testing settings (remove for production)
  users.users.craig.initialPassword = "test";
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "craig";

  # VM variant settings (only affects nixos-rebuild build-vm)
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
