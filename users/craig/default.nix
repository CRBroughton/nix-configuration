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

  # Font configuration
  fonts.fontconfig.enable = true;

  # Custom fonts
  home.file.".local/share/fonts/shavian" = {
    source = ../../fonts/shavian;
    recursive = true;
  };

}
