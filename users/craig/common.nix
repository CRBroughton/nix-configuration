{ pkgs, ... }:

{
  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # User configuration
  users.users.craig = {
    isNormalUser = true;
    description = "Craig";
    extraGroups = [
      "wheel"
      "networkmanager"
      "libvirtd"
      "audio"
      "video"
    ];
    initialPassword = "test";
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk"
    ];
  };
}
