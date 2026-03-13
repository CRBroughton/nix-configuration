{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.shell;
in
{
  options.shell = {
    enable = lib.mkEnableOption "fish shell with starship, zoxide, and CLI tools";
  };

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      shellAliases = {
        cd = "z";
        cat = "bat";
        ls = "eza";
      };
      shellInit = ''
        set -gx CGO_ENABLED 1
        set -gx PATH "$HOME/.local/share/pnpm" $PATH
      '';
      functions = {
        # Use local justfile if exists, otherwise fallback to nix-configuration
        just = ''
          if test -f justfile; or test -f .justfile; or test -f Justfile
            command just $argv
          else
            command just --justfile ~/nix-configuration/justfile --working-directory ~/nix-configuration $argv
          end
        '';
      };
    };

    programs.starship = {
      enable = true;
      enableFishIntegration = true;
    };

    programs.zoxide = {
      enable = true;
      enableFishIntegration = true;
    };

    home.packages = with pkgs; [
      bat
      eza
      btop
      ripgrep
      fd
      jq
      jnv
      tokei
      croc
    ];
  };
}
