{ inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

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
            users = lib.genAttrs [ user ] (u: import ../users/${u});
          };
        }
      ]
      ++ extraModules;
    };
}
