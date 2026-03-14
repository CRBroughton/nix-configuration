# SSH - Basic OpenSSH server
{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.ssh;
in
{
  options.modules.ssh = {
    enable = lib.mkEnableOption "OpenSSH server";
  };

  config = lib.mkIf cfg.enable {
    services.openssh.enable = true;
  };
}
