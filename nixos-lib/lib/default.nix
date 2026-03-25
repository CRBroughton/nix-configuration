{ inputs }:

let
  inherit (inputs)
    nixpkgs
    home-manager
    import-tree
    nix-format
    ;
  inherit (nixpkgs) lib;
in
rec {

  # Build a NixOS system for a single host.
  # Users are looked up from usersDir and injected by name.
  #
  # Example:
  #   mkHost {
  #     hostname = "pc";
  #     hostPath = ./hosts/pc;
  #     users = [ "demo" ];
  #     usersDir = ./users;
  #   }
  mkHost =
    {
      hostname,
      hostPath,
      users,
      usersDir,
      stateVersion,
      system ? "x86_64-linux",
      extraModules ? [ ],
      extraSpecialArgs ? { },
    }:
    let
      # Per-user system.nix files are imported as NixOS modules.
      # Use this for users.users.<name> = { ... } definitions.
      userSystemModules = lib.flatten (
        map (
          user:
          let
            systemFile = usersDir + "/${user}/system.nix";
          in
          lib.optional (builtins.pathExists systemFile) systemFile
        ) users
      );
    in
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit hostname; } // extraSpecialArgs;
      modules = [
        home-manager.nixosModules.home-manager
        hostPath
        {
          networking.hostName = hostname;
          nixpkgs.config.allowUnfree = true;
          system.stateVersion = stateVersion;

          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            # home.stateVersion is set globally — no need to set it per user.
            sharedModules = [ { home.stateVersion = stateVersion; } ];
            # Each entry in users becomes a home-manager user.
            # default.nix is the home-manager config; system.nix is the NixOS user definition.
            users = lib.genAttrs users (user: import (usersDir + "/${user}"));
          };
        }
      ]
      ++ userSystemModules
      ++ extraModules;
    };

  # Build nixosConfigurations from an explicit hosts attrset.
  # Users are shared across hosts — define once, inject anywhere.
  #
  # Example:
  #   mkFlake {
  #     inherit self;
  #     stateVersion = "25.11";
  #     hosts = {
  #       pc.users     = [ "demo" ];
  #       laptop.users = [ "demo" "alice" ];
  #     };
  #   }
  mkFlake =
    {
      self,
      stateVersion,
      hosts,
      hostsDir ? self + "/hosts",
      usersDir ? self + "/users",
      modulesDir ? self + "/modules",
      system ? "x86_64-linux",
      extraModules ? [ ],
      extraSpecialArgs ? { },
    }:
    let
      sharedModules = lib.optional (modulesDir != null) (import-tree modulesDir);
    in
    {
      nixosConfigurations = builtins.mapAttrs (
        hostname: hostCfg:
        mkHost {
          inherit hostname system extraSpecialArgs stateVersion usersDir;
          hostPath = hostsDir + "/${hostname}";
          users = hostCfg.users or [ ];
          extraModules = sharedModules ++ extraModules;
        }
      ) hosts;

      inherit (nix-format) devShells apps;
    };

}
