# Neovim via nix-modules (shared home-manager module)
{
  config,
  lib,
  user,
  ...
}:

let
  cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = {
    enable = lib.mkEnableOption "Neovim editor";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.neovim-modules = {
        enable = true;

        languages = {
          gameboy.enable = true;
          typescript.enable = true;
          vue.enable = true;
          go.enable = true;
          odin.enable = true;
          nix.enable = true;
          tailwind.enable = true;
        };

        plugins = {
          conventional-commit.enable = true;
          bufferline.enable = false;
          flash.enable = true;
          harpoon.enable = true;
          telescope.enable = true;
          theme.enable = true;
          which-key.enable = true;
        };
      };
    };
  };
}
