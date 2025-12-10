{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.lua;
in
{
  options.programs.lua = {
    enable = mkEnableOption "Lua environment with packages";

    extraPackages = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Lua package names to include";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (pkgs.lua.withPackages (ps: map (name: ps.${name}) cfg.extraPackages))
      pkgs.lua-language-server
    ];
  };
}
