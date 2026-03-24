{
  description = "NixOS configuration for laptop, gaming PC, and home server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    import-tree.url = "github:vic/import-tree";

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

    # Zen Browser Flatpak Configuration
    zen-flatpak-config = {
      url = "github:crbroughton/nix-flakes?dir=zen-flatpak-config";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Firefox addons for Zen Browser extensions
    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Podman user socket/service for rootless containers
    podman-flake = {
      url = "github:CRBroughton/nix-flakes?dir=podman-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Arion - Docker/Podman compose in Nix
    arion = {
      url = "github:hercules-ci/arion";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Dev tooling (nixfmt, statix, deadnix, etc.)
    nix-format = {
      url = "github:crbroughton/nix-flakes?dir=nix-format";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, import-tree, ... }@inputs:
    let
      myLib = import ./lib { inherit inputs; };

      # Auto-imports all .nix files in ./modules as NixOS modules
      modules = import-tree ./modules;

      stateVersion = "25.11";
    in
    {
      nixosConfigurations = {
        laptop = myLib.mkHost {
          hostname = "laptop";
          user = "craig";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        gaming-pc = myLib.mkHost {
          hostname = "gaming-pc";
          user = "craig";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        brighton-pc = myLib.mkHost {
          hostname = "brighton-pc";
          user = "craig";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        mum-pc = myLib.mkHost {
          hostname = "mum-pc";
          user = "mum";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        mums-laptop = myLib.mkHost {
          hostname = "mums-laptop";
          user = "mum";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        moons-pc = myLib.mkHost {
          hostname = "moons-pc";
          user = "moon";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        nixos-server = myLib.mkHost {
          hostname = "nixos-server";
          user = "craig";
          inherit stateVersion;
          extraModules = [ modules ];
        };

        pi-monitor = myLib.mkPi {
          hostname = "pi-monitor";
          user = "craig";
          inherit stateVersion;
          extraModules = [ ];
        };
      };
      images.pi-monitor = self.nixosConfigurations.pi-monitor.config.system.build.sdImage;

      inherit (inputs.nix-format) devShells;
      inherit (inputs.nix-format) apps;
    };
}
