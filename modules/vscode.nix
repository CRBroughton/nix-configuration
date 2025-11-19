{ config, pkgs, ... }:

{
  # VS Code (extensions managed by Nix)
  programs.vscode = {
    enable = true;
  };

  # Fix cursor size on Wayland with scaling
  xdg.desktopEntries.code = {
    name = "Visual Studio Code";
    genericName = "Text Editor";
    exec = "code --enable-features=WaylandWindowDecorations --ozone-platform=wayland %F";
    terminal = false;
    categories = [ "Utility" "TextEditor" "Development" "IDE" ];
    mimeType = [ "text/plain" "inode/directory" ];
    startupNotify = true;
    icon = "vscode";
  };
}
