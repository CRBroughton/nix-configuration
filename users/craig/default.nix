{
  ...
}:

{
  imports = [
    ./git.nix # Personal git config (name, email, keys)
    ./gnome.nix # Personal GNOME settings (theme, dock, extensions)
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

  # Modules
  shell.enable = true;
  git.enable = true;
  terminal.enable = true;
  media.enable = true;
  editors.vscode.enable = true;
  editors.neovim.enable = true;
  editors.zed.enable = true;
  browsers.zen.enable = true;
}
