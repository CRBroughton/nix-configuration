{ inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;
in
{
  # Create a NixOS system configuration
  mkHost = {
    hostname,
    system ? "x86_64-linux",
    users ? [ "craig" ],
    extraModules ? [],
  }:
  inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs hostname; };
    modules = [
      # External modules
      inputs.chaotic.nixosModules.default
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.nix-flatpak.nixosModules.nix-flatpak

      # Host-specific config
      ../hosts/${hostname}

      # Base configuration
      {
        networking.hostName = hostname;

        nixpkgs = {
          config.allowUnfree = true;
          overlays = [
            inputs.nix-vscode-extensions.overlays.default
          ];
        };

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = { inherit inputs; };
          users = lib.genAttrs users (user: import ../users/${user});
        };
      }
    ] ++ extraModules;
  };
}