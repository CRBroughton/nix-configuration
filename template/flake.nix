{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Dev tooling (nixfmt, statix, deadnix, nil, nix-format script)
    nix-format = {
      url = "github:crbroughton/nix-flakes?dir=nix-format";
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

      # Dev tooling - enter with: nix develop
      inherit (inputs.nix-format) devShells;
      inherit (inputs.nix-format) apps;
    };
}
