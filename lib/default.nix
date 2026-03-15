{ inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

  # Path shortcuts for cleaner imports
  paths = {
    modules = ../modules;
    disko = ../disko;
  };
in
{
  # Create a NixOS system configuration
  # Hosts with a users/${user}/hosts/${hostname}/home.nix get that as their HM config;
  # otherwise falls back to users/${user}/default.nix
  mkHost =
    {
      hostname,
      user,
      stateVersion,
      system ? "x86_64-linux",
      extraModules ? [ ],
    }:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs hostname user;
        inherit (inputs) firefox-addons;
      }
      // paths;
      modules = [
        # External modules
        inputs.chaotic.nixosModules.default
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
        inputs.nix-flatpak.nixosModules.nix-flatpak
        inputs.arion.nixosModules.arion

        # Shared modules
        ../modules/common.nix

        # Host-specific config (under user directory)
        ../users/${user}/hosts/${hostname}

        # Base configuration
        {
          networking.hostName = hostname;
          system.stateVersion = stateVersion;

          nixpkgs = {
            config.allowUnfree = true;
            overlays = [
              inputs.nix-vscode-extensions.overlays.default
            ];
          };

          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = {
              inherit inputs;
            };
            sharedModules = [
              inputs.zen-flatpak-config.homeManagerModules.default
              inputs.podman-flake.homeManagerModules.default
              { home.stateVersion = stateVersion; }
            ];
            users = lib.genAttrs [ user ] (
              u:
              let
                hostHome = ../users/${u}/hosts/${hostname}/home.nix;
              in
              if builtins.pathExists hostHome then import hostHome else import ../users/${u}
            );
          };
        }
      ]
      ++ extraModules;
    };

  # Create a minimal Pi configuration (no home-manager, aarch64)
  mkPi =
    {
      hostname,
      user,
      stateVersion,
      extraModules ? [ ],
    }:
    inputs.nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      specialArgs = {
        inherit inputs hostname user;
      }
      // paths;
      modules = [
        # Shared modules
        ../modules/common.nix

        # Host-specific config (under user directory)
        ../users/${user}/hosts/${hostname}

        # Base configuration
        {
          nixpkgs.config.allowUnfree = true;
          system.stateVersion = stateVersion;
        }
      ]
      ++ extraModules;
    };
}
