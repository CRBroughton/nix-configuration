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
  # Hosts live in hosts/<hostname>/; users in users/<username>/.
  # Each user's common.nix (NixOS user definition) is auto-imported.
  # Per-host HM overrides go in hosts/<hostname>/users/<username>/home.nix;
  # otherwise falls back to users/<username>/default.nix.
  mkHost =
    {
      hostname,
      users, # list of usernames, e.g. [ "craig" ]
      stateVersion,
      system ? "x86_64-linux",
      extraModules ? [ ],
    }:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs hostname users;
        user = builtins.head users; # primary user — for module backwards-compat
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

        # Host-specific config
        ../hosts/${hostname}

        # Auto-import each user's NixOS user definition
        { imports = map (u: ../users/${u}/common.nix) users; }

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
              inherit (inputs) firefox-addons;
            };
            sharedModules = [
              inputs.zen-flatpak-config.homeManagerModules.default
              inputs.podman-flake.homeManagerModules.default
              { home.stateVersion = stateVersion; }
            ];
            users = lib.genAttrs users (
              u:
              let
                hostHome = ../hosts/${hostname}/users/${u}/home.nix;
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

        # Host-specific config
        ../hosts/${hostname}

        # Base configuration
        {
          nixpkgs.config.allowUnfree = true;
          system.stateVersion = stateVersion;
        }
      ]
      ++ extraModules;
    };
}
