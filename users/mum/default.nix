{ pkgs, ... }:

{
  imports = [
    ./gnome.nix
    ./zen.nix
  ];

  home.homeDirectory = "/home/mum";

  home.packages = with pkgs; [
    obs-studio
    git
    wayvnc
  ];

  xdg.configFile."wayvnc/config".text = ''
    enable_auth=false
    address=0.0.0.0
    port=5900
  '';

  # Set GNOME Remote Desktop RDP credentials from agenix secret on each login
  systemd.user.services.gnome-rdp-credentials = {
    Unit = {
      Description = "Set GNOME Remote Desktop credentials";
      After = [ "gnome-remote-desktop.service" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.gnome-remote-desktop}/bin/grdctl rdp enable && ${pkgs.gnome-remote-desktop}/bin/grdctl rdp set-credentials mum $(cat /run/agenix/mum-rdp-password)'";
      RemainAfterExit = true;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
