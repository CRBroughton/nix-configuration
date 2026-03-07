{ inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;
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
    specialArgs = { inherit inputs hostname; };
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
}
