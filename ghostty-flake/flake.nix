{
  description = "Ghostty terminal with nixGL wrapper for non-NixOS systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghostty.url = "github:ghostty-org/ghostty";
    nixgl.url = "github:nix-community/nixGL";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      ghostty,
      nixgl,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        glWrapper = nixgl.packages.${system}.nixGLIntel;

        wrappedGhostty = pkgs.writeShellScriptBin "ghostty" ''
          exec ${glWrapper}/bin/nixGLIntel ${ghostty.packages.${system}.default}/bin/ghostty "$@"
        '';

        ghostty-wrapped = pkgs.stdenv.mkDerivation {
          name = "ghostty-wrapped";
          version = "latest";

          dontUnpack = true;

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/applications
            mkdir -p $out/share/icons/hicolor/128x128/apps
            mkdir -p $out/share/icons/hicolor/scalable/apps

            # Install the wrapped executable
            cp ${wrappedGhostty}/bin/ghostty $out/bin/ghostty

            # Copy icons (try multiple sizes)
            cp ${
              ghostty.packages.${system}.default
            }/share/icons/hicolor/128x128/apps/com.mitchellh.ghostty.png \
               $out/share/icons/hicolor/128x128/apps/ghostty.png 2>/dev/null || \
            cp ${
              ghostty.packages.${system}.default
            }/share/icons/hicolor/128x128/apps/com.mitchellh.ghostty.png \
               $out/share/icons/hicolor/128x128/apps/com.mitchellh.ghostty.png 2>/dev/null || true

            # Try to copy SVG icon if available
            cp ${
              ghostty.packages.${system}.default
            }/share/icons/hicolor/scalable/apps/com.mitchellh.ghostty.svg \
               $out/share/icons/hicolor/scalable/apps/ghostty.svg 2>/dev/null || true

            # Create desktop entry with absolute path
            cat > $out/share/applications/ghostty.desktop <<EOF
            [Desktop Entry]
            Version=1.0
            Type=Application
            Name=Ghostty
            Comment=Fast, native, feature-rich terminal emulator
            Exec=$out/bin/ghostty
            Icon=com.mitchellh.ghostty
            Terminal=false
            Categories=System;TerminalEmulator;
            Keywords=terminal;tty;
            StartupNotify=true
            StartupWMClass=com.mitchellh.ghostty
            EOF
          '';
        };

      in
      {
        packages = {
          default = ghostty-wrapped;
          ghostty = ghostty-wrapped;
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
          options.programs.ghostty-wrapped = {
            enable = lib.mkEnableOption "Ghostty terminal with nixGL wrapper";

            settings = lib.mkOption {
              type = lib.types.attrs;
              default = { };
              description = "Ghostty configuration options";
            };

            glWrapper = lib.mkOption {
              type = lib.types.enum [
                "nixGLIntel"
                "nixGLNvidia"
                "nixGLMesa"
              ];
              default = "nixGLIntel";
              description = "Which nixGL wrapper to use";
            };
          };

          config = lib.mkIf config.programs.ghostty-wrapped.enable (
            let
              ghostty-pkg = self.packages.${system}.default;
              ghostty-upstream = self.inputs.ghostty.packages.${system}.default;
            in
            {
              home.packages = [ ghostty-pkg ];

              # Explicitly create desktop entry in ~/.local/share
              xdg.dataFile."applications/ghostty.desktop".source =
                "${ghostty-pkg}/share/applications/ghostty.desktop";

              # Copy icon files
              xdg.dataFile."icons/hicolor/128x128/apps/com.mitchellh.ghostty.png".source =
                "${ghostty-upstream}/share/icons/hicolor/128x128/apps/com.mitchellh.ghostty.png";

              # Config file
              xdg.configFile."ghostty/config" = lib.mkIf (config.programs.ghostty-wrapped.settings != { }) {
                text = lib.concatStringsSep "\n" (
                  lib.mapAttrsToList (
                    name: value: "${name} = ${toString value}"
                  ) config.programs.ghostty-wrapped.settings
                );
              };
            }
          );
        };
    };
}
