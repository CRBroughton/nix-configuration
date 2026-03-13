{ inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

  # Auto-imported home-manager modules (available to all users)
  homeModules = inputs.import-tree ../modules/_home;
in
{
  mkHost =
    {
      hostname,
      user,
      system ? "x86_64-linux",
      extraModules ? [ ],
    }:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs hostname user;
      };
      modules = [
        inputs.home-manager.nixosModules.home-manager

        # Host-specific config
        ../users/${user}/hosts/${hostname}

        # Base configuration
        {
          networking.hostName = hostname;
          nixpkgs.config.allowUnfree = true;

          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = { inherit inputs; };
            sharedModules = [ homeModules ];
            users = lib.genAttrs [ user ] (u: import ../users/${u});
          };
        }
      ]
      ++ extraModules;
    };
}
