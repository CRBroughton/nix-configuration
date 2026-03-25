# Home-manager configuration for the demo user.
# stateVersion is set globally by mkFlake — no need to set it here.
{ pkgs, ... }:

{
  # User-level packages (available only to this user)
  home.packages = with pkgs; [
    ripgrep
    fd
    jq
  ];

  # Dotfiles / XDG config
  # home.file.".config/foo/config".text = ''...'';

  # Program modules — home-manager has built-in options for many tools
  programs.git = {
    enable = true;
    userName = "Demo User";
    userEmail = "demo@example.com";
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      ll = "ls -la";
      ".." = "cd ..";
    };
  };
}
