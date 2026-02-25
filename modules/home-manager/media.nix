{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    qpwgraph
    picard
    element-desktop
    mumble
  ];
}
