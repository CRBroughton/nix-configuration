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
          typescript.enable = true;
          vue.enable = true;
          go.enable = true;
          odin.enable = true;
          nix.enable = true;
          tailwind.enable = true;
        };

        plugins = {
          telescope.enable = true;
          theme.enable = true;
        };
      };
    };
  };
}
