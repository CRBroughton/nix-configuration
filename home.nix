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
    ./modules/create-laravel-api.nix
    ./modules/laravel-api-cleanup.nix
    ./modules/go.nix
    ./modules/zig.nix
    ./modules/node.nix
    ./modules/keyboard.nix
    ./modules/lazydocker.nix
    ./modules/ssh.nix
    ./modules/git.nix
    ./modules/posting.nix
    ./modules/zen-browser.nix
    ./modules/pia-vpn.nix
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

  programs.go-installer = {
    enable = true;
  };

  programs.zig-installer = {
    enable = true;
  };

  programs.node-installer = {
    enable = true;
    # version = "18.18.0";  # Uncomment to use a specific version
  };

  programs.lua.enable = true;
  programs.love2d.enable = true;

  # Automatic garbage collection via systemd timer
  systemd.user.timers.nix-gc = {
    Unit = {
      Description = "Automatic Nix Garbage Collection";
    };
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  systemd.user.services.nix-gc = {
    Unit = {
      Description = "Nix Garbage Collection";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.writeShellScript "nix-gc" ''
        ${pkgs.home-manager}/bin/home-manager expire-generations "-30 days"
        ${pkgs.nix}/bin/nix-collect-garbage --delete-old
      ''}";
    };
  };
}
