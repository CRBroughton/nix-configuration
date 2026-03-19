{ pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.mum = {
    isNormalUser = true;
    description = "Mum";
    extraGroups = [
      "networkmanager"
      "audio"
      "video"
    ];
    initialPassword = "test";
    shell = pkgs.bash;
  };
}
