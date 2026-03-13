{
  ...
}:

{
  imports = [
    ../../modules/_home/shell.nix
    ../../modules/_home/git.nix
    ./git.nix # Personal git config (name, email, keys)
    ./gnome.nix # Personal GNOME settings (theme, dock, extensions)
    ../../modules/_home/terminal.nix
    ../../modules/_home/media.nix
    ../../modules/_home/editors/vscode.nix
    ../../modules/_home/editors/neovim.nix
    ../../modules/_home/editors/zed.nix
    ../../modules/_home/zen-browser.nix
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
