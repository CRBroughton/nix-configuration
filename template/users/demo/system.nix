# NixOS user definition for demo
{ pkgs, ... }:

{
  users.users.demo = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    shell = pkgs.bash;
  };
}
