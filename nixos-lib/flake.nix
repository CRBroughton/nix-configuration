{
  description = "Reusable NixOS configuration library";

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
    {
      nixpkgs,
      home-manager,
      import-tree,
      nix-format,
      ...
    }:
    {
      lib = import ./lib {
        inputs = {
          inherit
            nixpkgs
            home-manager
            import-tree
            nix-format
            ;
        };
      };
    };
}
