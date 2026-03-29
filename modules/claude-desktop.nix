{
  inputs,
  pkgs,
  lib,
  config,
  ...
}:

{
  options.modules.claude-desktop.enable = lib.mkEnableOption "Claude Desktop app";

  config = lib.mkIf config.modules.claude-desktop.enable {
    nixpkgs.overlays = [ inputs.claude-desktop.overlays.default ];
    environment.systemPackages = [ pkgs.claude-desktop ];
  };
}
