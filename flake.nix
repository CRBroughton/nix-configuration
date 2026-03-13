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
      # Files in _home/ and _server/ are skipped by import-tree
      modules = import-tree ./modules;

      # Server-specific modules (arion options only available in mkServer)
      serverModules = import-tree ./modules/_server;
    in
    {
      nixosConfigurations = {
        laptop = myLib.mkHost {
          hostname = "laptop";
          user = "craig";
          extraModules = [ modules ];
        };

        gaming-pc = myLib.mkHost {
          hostname = "gaming-pc";
          user = "craig";
          extraModules = [ modules ];
        };

        nixos-server = myLib.mkServer {
          hostname = "nixos-server";
          user = "craig";
          extraModules = [ serverModules ];
        };

        pi-monitor = myLib.mkPi {
          hostname = "pi-monitor";
          user = "craig";
          extraModules = [ ];
        };
      };
      images.pi-monitor = self.nixosConfigurations.pi-monitor.config.system.build.sdImage;

      inherit (inputs.nix-format) devShells;
      inherit (inputs.nix-format) apps;
    };
}
