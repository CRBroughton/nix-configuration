{ config, pkgs, ... }:

{
  imports = [
    ./modules/packages.nix
    ./modules/fonts.nix
    ./modules/shell.nix
    ./modules/vscode.nix
    ./modules/vscode-extensions.nix
    ./modules/vscode-settings.nix
    ./modules/ghostty.nix
    ./modules/flatpak.nix
    ./modules/gnome.nix
    ./modules/gnome-extensions-installer.nix
    ./modules/keyboard.nix
    ./modules/lazydocker.nix
    ./modules/ssh.nix
    ./modules/git.nix
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "craig";
  home.homeDirectory = "/home/craig";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Allow unfree packages (needed for VS Code, etc.)
  nixpkgs.config.allowUnfree = true;

  # Font configuration
  fonts.fontconfig.enable = true;

  # Copy fonts from the parent directory
  # home.file.".local/share/fonts/shavian" = {
  #   source = ../files/fonts;
  #   recursive = true;
  # };

  # Copy setup scripts
  # home.file.".config/home-manager/scripts" = {
  #   source = ../files/scripts;
  #   recursive = true;
  # };

  # systemd user services
  # systemd.user.services.pcscd = {
  #   Unit = {
  #     Description = "PC/SC Smart Card Daemon";
  #     After = [ "sockets.target" ];
  #   };
  #   Service = {
  #     ExecStart = "${pkgs.pcsclite}/bin/pcscd --foreground --auto-exit";
  #     Restart = "on-failure";
  #   };
  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  # };
}
