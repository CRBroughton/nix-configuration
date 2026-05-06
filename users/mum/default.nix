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
    rustdesk
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
      Environment = [
        "WAYLAND_DISPLAY=wayland-1"
        "XDG_RUNTIME_DIR=/run/user/1000"
      ];
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
