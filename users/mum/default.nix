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

  systemd.user.services.wayvnc = {
    Unit = {
      Description = "WayVNC remote desktop server";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.wayvnc}/bin/wayvnc";
      Restart = "on-failure";
      RestartSec = "5s";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
