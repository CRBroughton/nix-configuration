{
  description = "Nix dev tooling - nixd, nil, statix, deadnix, nixfmt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nix-format = pkgs.writeShellScriptBin "nix-format" ''
          echo "Formatting all Nix files..."
          find . -name '*.nix' -type f -exec ${pkgs.nixfmt}/bin/nixfmt {} +
          ${pkgs.statix}/bin/statix fix
          ${pkgs.deadnix}/bin/deadnix --edit .
          echo "Done"
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            nixd
            nil
            nixfmt
            statix
            deadnix
            nix-format
          ];
        };

        apps.format = {
          type = "app";
          meta.description = "Format and lint all Nix files";
          program = "${nix-format}/bin/nix-format";
        };
      }
    );
}
