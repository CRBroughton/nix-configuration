{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../modules/home-manager/shell.nix
    ../../modules/home-manager/git.nix
    ../../modules/home-manager/terminal.nix
    ../../modules/home-manager/gnome.nix
    ../../modules/home-manager/development.nix
    ../../modules/home-manager/gaming.nix
    ../../modules/home-manager/media.nix
    ../../modules/home-manager/security.nix
    ../../modules/home-manager/editors/vscode.nix
    ../../modules/home-manager/editors/neovim.nix
    ../../modules/home-manager/editors/zed.nix
  ];

  home.username = "craig";
  home.homeDirectory = "/home/craig";
  home.stateVersion = "24.11";

  # Font configuration
  fonts.fontconfig.enable = true;

  # Custom fonts
  home.file.".local/share/fonts/shavian" = {
    source = ../../fonts/shavian;
    recursive = true;
  };
}
