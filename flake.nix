{
  description = "NixOS configuration for laptop and gaming PC";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # CachyOS kernel and optimizations
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    # VSCode extensions from marketplace
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";

    # Declarative flatpak
    nix-flatpak.url = "github:gmodena/nix-flatpak";
  };

  outputs = { self, nixpkgs, home-manager, disko, chaotic, nix-vscode-extensions, nix-flatpak, ... }@inputs:
  let
    myLib = import ./lib { inherit inputs; };
  in {
    nixosConfigurations = {
      laptop = myLib.mkHost {
        hostname = "laptop";
      };

      gaming-pc = myLib.mkHost {
        hostname = "gaming-pc";
      };
    };
  };
}
