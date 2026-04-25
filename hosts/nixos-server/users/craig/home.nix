# Home-manager identity config for nixos-server
{ ... }:

{
  imports = [
    ../../../../users/craig/git.nix # Personal git config
  ];

  home.username = "craig";
  home.homeDirectory = "/home/craig";
}
