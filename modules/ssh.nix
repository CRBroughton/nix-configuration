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

  services.ssh-agent.enable = true;

  home.file.".ssh/id_ed25519.pub" = {
    text = ''
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG6fDKEifgPIGajJMXb7bZX0IiEUlbqLWxNt8dX2er8+ craig@pop-os
    '';
  };

  # Use the following to set the key to the proper permissions
  # chmod 600 ~/.ssh/id_ed25519
}