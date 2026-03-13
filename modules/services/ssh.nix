# SSH - Basic OpenSSH server
{
  config,
  lib,
  ...
}:

let
  cfg = config.services.sshServer;
in
{
  options.services.sshServer = {
    enable = lib.mkEnableOption "OpenSSH server";
  };

  config = lib.mkIf cfg.enable {
    services.openssh.enable = true;
  };
}
