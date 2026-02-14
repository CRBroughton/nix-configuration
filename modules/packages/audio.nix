{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Audio
    qpwgraph
    picard
    element-desktop
    mumble
  ];
}
