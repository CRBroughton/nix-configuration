{
  description = "Home Manager configuration for Craig";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-flatpak.url = "github:gmodena/nix-flatpak";

    # Ghostty with nixGL wrapper
    ghostty-wrapped.url = "path:./ghostty-flake";

    # Fresh editor
    fresh-editor.url = "path:./fresh-flake";

    # Zed editor with Vulkan dependencies
    zed-wrapped.url = "path:./zed-flake";

    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";

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

    lua-dev = {
      url = "github:crbroughton/nix-flakes?dir=lua";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    love2d = {
      url = "github:crbroughton/nix-flakes?dir=love2d";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nix-flatpak,
      ghostty-wrapped,
      fresh-editor,
      zed-wrapped,
      nix-vscode-extensions,
      zen-flatpak-config,
      firefox-addons,
      lua-dev,
      love2d,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          nix-vscode-extensions.overlays.default
        ];
      };
    in
    {
      homeConfigurations."craig" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          nix-flatpak.homeManagerModules.nix-flatpak
          ghostty-wrapped.homeManagerModules.default
          fresh-editor.homeManagerModules.default
          zed-wrapped.homeManagerModules.default
          lua-dev.homeManagerModules.default
          love2d.homeManagerModules.default
          zen-flatpak-config.homeManagerModules.default
          {
            _module.args = {
              inherit firefox-addons;
            };
          }
          ./home.nix
        ];
      };
    };
}
