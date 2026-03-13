# SSH - Hardened server configuration
{ config, lib, ... }:

let cfg = config.server.ssh; in
{
  options.server.ssh = {
    enable = lib.mkEnableOption "hardened OpenSSH server";
  };

  config = lib.mkIf cfg.enable {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        X11Forwarding = false;
      };
    };
  };
}
