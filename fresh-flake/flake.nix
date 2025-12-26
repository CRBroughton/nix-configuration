{
  description = "Fresh editor AppImage for non-NixOS systems";

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

        fresh-editor = pkgs.appimageTools.wrapType2 {
          name = "fresh-editor";
          version = "0.1.64";

          src = pkgs.fetchurl {
            url = "https://github.com/sinelaw/fresh/releases/download/v0.1.64/fresh-editor-0.1.64-x86_64.AppImage";
            sha256 = "00lbyyb8rpa6nwi2x72p6vkvmcydwc7rsb89mj7d72zdm0yi4502";
          };

          extraPkgs =
            pkgs: with pkgs; [
              # Add any dependencies the AppImage might need
            ];
        };

        fresh-wrapped = pkgs.stdenv.mkDerivation {
          name = "fresh-editor-wrapped";
          version = "0.1.64";

          dontUnpack = true;

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/applications
            mkdir -p $out/share/icons/hicolor/128x128/apps

            # Install the wrapped executable
            ln -s ${fresh-editor}/bin/fresh-editor $out/bin/fresh

            # Create desktop entry
            cat > $out/share/applications/fresh.desktop <<EOF
            [Desktop Entry]
            Version=1.0
            Type=Application
            Name=Fresh
            Comment=A fresh take on the terminal-based editor
            Exec=$out/bin/fresh %F
            Icon=fresh
            Terminal=false
            Categories=Development;TextEditor;Utility;
            Keywords=editor;text;code;
            StartupNotify=true
            MimeType=text/plain;text/x-c;text/x-c++;text/x-java;text/x-python;
            EOF
          '';
        };

      in
      {
        packages = {
          default = fresh-wrapped;
          fresh = fresh-wrapped;
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
          options.programs.fresh-editor = {
            enable = lib.mkEnableOption "Fresh editor";
          };

          config = lib.mkIf config.programs.fresh-editor.enable (
            let
              fresh-pkg = self.packages.${system}.default;
            in
            {
              home.packages = [ fresh-pkg ];

              # Explicitly create desktop entry in ~/.local/share
              xdg.dataFile."applications/fresh.desktop".source =
                "${fresh-pkg}/share/applications/fresh.desktop";
            }
          );
        };
    };
}