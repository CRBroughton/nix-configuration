{ pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.moon = {
    isNormalUser = true;
    description = "Moon";
    extraGroups = [
      "networkmanager"
      "audio"
      "video"
    ];
    initialPassword = "test";
    shell = pkgs.bash;
  };
}
