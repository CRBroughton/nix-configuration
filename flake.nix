{
  description = "Home Manager configuration for Craig";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-flatpak.url = "github:gmodena/nix-flatpak";

    # Ghostty with nixGL wrapper
    ghostty-wrapped.url = "path:./ghostty-flake";

    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  };

  outputs = { nixpkgs, home-manager, nix-flatpak, ghostty-wrapped, nix-vscode-extensions, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          nix-vscode-extensions.overlays.default
        ];
      };
    in {
      homeConfigurations."craig" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          nix-flatpak.homeManagerModules.nix-flatpak
          ghostty-wrapped.homeManagerModules.default
          ./home.nix
        ];
      };
    };
}
