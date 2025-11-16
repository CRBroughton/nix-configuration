{ config, pkgs, ... }:

{
  # VS Code (extensions managed by Nix)
  programs.vscode = {
    enable = true;
  };
}
