{
  config,
  lib,
  user,
  ...
}:

let
  cfg = config.modules.zellij;
in
{
  options.modules.zellij = {
    enable = lib.mkEnableOption "Zellij terminal multiplexer";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.zellij-modules.enable = true;
    };
  };
}
