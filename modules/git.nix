{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "CRBroughton";
    userEmail = "crbroughton@posteo.uk";

    signing = {
      key = "A28B475638A29DA4ACB36D69B53E1BC28B18FC17";
      signByDefault = true;
    };

    extraConfig = {
      init = {
        defaultBranch = "master";
      };
    };
  };
}
