{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    let
      myLib = import ./lib { inherit inputs; };
    in
    {
      nixosConfigurations = {
        # Rename this to match your hostname
        hostname = myLib.mkHost {
          hostname = "pc";
          user = "demo";
        };
      };
    };
}
