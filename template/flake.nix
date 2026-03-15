{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    import-tree.url = "github:vic/import-tree";

    # Dev tooling (nixfmt, statix, deadnix, nil, nix-format script)
    nix-format = {
      url = "github:crbroughton/nix-flakes?dir=nix-format";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { import-tree, ... }@inputs:
    let
      myLib = import ./lib { inherit inputs; };

      # Auto-imports all .nix files in ./modules as NixOS modules
      # Files in _home/ are skipped (home-manager modules live there)
      modules = import-tree ./modules;
    in
    {
      nixosConfigurations = {
        # Rename this to match your hostname
        hostname = myLib.mkHost {
          hostname = "pc";
          user = "demo";
          extraModules = [ modules ];
        };
      };

      # Dev tooling - enter with: nix develop
      inherit (inputs.nix-format) devShells;
      inherit (inputs.nix-format) apps;
    };
}
