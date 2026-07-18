{
  config,
  lib,
  user,
  ...
}:

let
  cfg = config.modules.editors.helix;
in
{
  options.modules.editors.helix = {
    enable = lib.mkEnableOption "Helix editor";

    languages = {
      typescript.enable = lib.mkEnableOption "TypeScript/TSX" // { default = true; };
      vue.enable        = lib.mkEnableOption "Vue"            // { default = true; };
      go.enable         = lib.mkEnableOption "Go"             // { default = true; };
      odin.enable       = lib.mkEnableOption "Odin"           // { default = true; };
      nix.enable        = lib.mkEnableOption "Nix"            // { default = true; };
      tailwind.enable   = lib.mkEnableOption "Tailwind CSS"   // { default = true; };
      unocss.enable     = lib.mkEnableOption "UnoCSS"         // { default = true; };
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.helix-modules = {
        enable = true;
        languages = cfg.languages;
      };
    };
  };
}
