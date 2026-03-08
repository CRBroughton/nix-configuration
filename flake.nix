{
  description = "NixOS configuration for laptop, gaming PC, and home server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
  };

  outputs = { self, nixpkgs, home-manager, disko, chaotic, nix-vscode-extensions, nix-flatpak, ... }@inputs:
  let
    myLib = import ./lib { inherit inputs; };
  in {
    nixosConfigurations = {
      laptop = myLib.mkHost {
        hostname = "laptop";
        user = "craig";
      };

      gaming-pc = myLib.mkHost {
        hostname = "gaming-pc";
        user = "craig";
      };

      nixos-server = myLib.mkServer {
        hostname = "nixos-server";
        user = "craig";
      };

      pi-monitor = myLib.mkPi {
        hostname = "pi-monitor";
        user = "craig";
      };
    };

    # Build SD card images with: nix build .#images.pi-monitor
    images.pi-monitor = self.nixosConfigurations.pi-monitor.config.system.build.sdImage;
  };
}
