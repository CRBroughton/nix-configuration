{
  description = "NixOS configuration";

  # nixos-lib tracks nixos-unstable by default. To pin to a stable release:
  #
  #   inputs = {
  #     nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  #     nixos-lib = {
  #       url = "path:../nixos-lib";
  #       inputs.nixpkgs.follows = "nixpkgs";
  #       inputs.home-manager.url = "github:nix-community/home-manager/release-26.05";
  #       inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  #     };
  #   };
  #
  inputs.nixos-lib.url = "path:../nixos-lib";

  outputs =
    { self, nixos-lib, ... }:
    nixos-lib.lib.mkFlake {
      inherit self;
      stateVersion = "25.11";
      hosts = {
        pc.users = [ "demo" ];
      };
    };
}
