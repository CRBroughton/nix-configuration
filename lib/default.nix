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
  mkHost = {
    hostname,
    user,                    # Required: the user who owns this host
    system ? "x86_64-linux",
    extraModules ? [],
  }:
  inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs hostname user; } // paths;
    modules = [
      # External modules
      inputs.chaotic.nixosModules.default
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.nix-flatpak.nixosModules.nix-flatpak

      # Host-specific config (under user directory)
      ../users/${user}/hosts/${hostname}

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
          sharedModules = [
            inputs.zen-flatpak-config.homeManagerModules.default
            { _module.args.firefox-addons = inputs.firefox-addons; }
          ];
          users = lib.genAttrs [ user ] (u: import ../users/${u});
        };
      }
    ] ++ extraModules;
  };

  # Create a server configuration (headless, no desktop modules)
  mkServer = {
    hostname,
    user,
    system ? "x86_64-linux",
    extraModules ? [],
  }:
  inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs hostname user; } // paths;
    modules = [
      # External modules (server doesn't need chaotic/flatpak)
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.arion.nixosModules.arion

      # Host-specific config (under user directory)
      ../users/${user}/hosts/${hostname}

      # Base configuration
      {
        networking.hostName = hostname;

        nixpkgs = {
          config.allowUnfree = true;
        };

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = { inherit inputs; };
          sharedModules = [
            inputs.podman-flake.homeManagerModules.default
          ];
          # Use host-specific home.nix for servers
          users = lib.genAttrs [ user ] (u: import ../users/${u}/hosts/${hostname}/home.nix);
        };
      }
    ] ++ extraModules;
  };

  # Create a minimal Pi configuration (no home-manager, aarch64)
  mkPi = {
    hostname,
    user,
    extraModules ? [],
  }:
  inputs.nixpkgs.lib.nixosSystem {
    system = "aarch64-linux";
    specialArgs = { inherit inputs hostname user; } // paths;
    modules = [
      # Host-specific config (under user directory)
      ../users/${user}/hosts/${hostname}

      # Base configuration
      {
        nixpkgs.config.allowUnfree = true;
      }
    ] ++ extraModules;
  };
}
