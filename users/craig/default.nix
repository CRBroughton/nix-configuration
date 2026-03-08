{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../modules/shell.nix
    ../../modules/git.nix
    ./git.nix   # Personal git config (name, email, keys)
    ./gnome.nix # Personal GNOME settings (theme, dock, extensions)
    ../../modules/terminal.nix
    ../../modules/media.nix
    ../../modules/editors/vscode.nix
    ../../modules/editors/neovim.nix
    ../../modules/editors/zed.nix
    ../../modules/zen-browser.nix
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
