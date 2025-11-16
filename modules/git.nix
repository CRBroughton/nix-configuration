{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    signing = {
      key = "A28B475638A29DA4ACB36D69B53E1BC28B18FC17";
      signByDefault = true;
    };

    settings = {
      user = {
        name = "CRBroughton";
        email = "crbroughton@posteo.uk";
      };
      init = {
        defaultBranch = "master";
      };
    };
  };
}
