{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    qpwgraph
    picard
    mumble
  ];
}
