{ config, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };

  home.file.".ssh/id_ed25519.pub" = {
    text = ''
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG6fDKEifgPIGajJMXb7bZX0IiEUlbqLWxNt8dX2er8+ craig@pop-os
    '';
  };
}