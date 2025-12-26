{
  description = "Zed editor with Vulkan dependencies for non-NixOS systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        zed-with-deps = pkgs.symlinkJoin {
          name = "zed-editor";
          paths = [ pkgs.zed-editor ];
          buildInputs = with pkgs; [
            vulkan-tools
            vulkan-loader
            vulkan-validation-layers
            mesa
          ];
        };

      in
      {
        packages = {
          default = zed-with-deps;
          zed = zed-with-deps;
        };
      }
    )
    // {
      # Export Home Manager module at the top level
      homeManagerModules.default =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          system = pkgs.stdenv.hostPlatform.system;
        in
        {
          options.programs.zed-editor = {
            enable = lib.mkEnableOption "Zed editor with Vulkan dependencies";
          };

          config = lib.mkIf config.programs.zed-editor.enable {
            home.packages = [
              self.packages.${system}.default
              pkgs.vulkan-tools
              pkgs.vulkan-loader
              pkgs.vulkan-validation-layers
              pkgs.mesa
            ];
          };
        };
    };
}
