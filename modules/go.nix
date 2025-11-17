{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.go-installer;

  # UPDATE THIS when new Go versions are released
  latestStableVersion = "1.25.4";

  # Function to create a Go derivation for a specific version
  buildGo = { version, sha256 }:
    pkgs.stdenv.mkDerivation {
      pname = "go";
      inherit version;

      src = pkgs.fetchurl {
        url = "https://go.dev/dl/go${version}.linux-amd64.tar.gz";
        inherit sha256;
      };

      # No build needed, just extract and install
      dontBuild = true;
      dontUnpack = true;

      installPhase = ''
        runHook preInstall

        mkdir -p $out/lib/go
        ${pkgs.gnutar}/bin/tar -xzf $src -C $out/lib/go --strip-components=1

        # Symlink binaries to bin/
        mkdir -p $out/bin
        ln -s $out/lib/go/bin/* $out/bin/

        runHook postInstall
      '';

      meta = with lib; {
        description = "The Go Programming Language";
        homepage = "https://go.dev/";
        license = licenses.bsd3;
        platforms = [ "x86_64-linux" ];
      };
    };

  # Predefined versions with their hashes
  # To get hash for a new version, run:
  # nix-prefetch-url https://go.dev/dl/go1.25.4.linux-amd64.tar.gz
  goVersions = {
    "1.25.4" = {
      version = "1.25.4";
      sha256 = "1v0vr9cn2pg3q4ry28acfax22r229s10zapkcw7yc38plknzz9cz";
    };
    "1.23.4" = {
      version = "1.23.4";
      sha256 = "0w89d3akbfifpjbl34zv0bx6wikxj74rv4kfcxvy4vz8bpgfy939";
    };
    "1.22.10" = {
      version = "1.22.10";
      sha256 = "14i04nrbyvdd83ncmm9fcldxkk47c0i22qcsf696lxcxl69f8v3k";
    };
  };

  selectedVersion = if cfg.version == "latest"
    then latestStableVersion
    else cfg.version;

  versionConfig = goVersions.${selectedVersion} or (throw ''
    Go version ${selectedVersion} is not defined.
    Available versions: ${concatStringsSep ", " (attrNames goVersions)}

    To add a new version, get its hash by running:
      nix-prefetch-url https://go.dev/dl/go${selectedVersion}.linux-amd64.tar.gz

    Then add it to goVersions in modules/go.nix
  '');

  goPackage = buildGo versionConfig;
in
{
  options.programs.go-installer = {
    enable = mkEnableOption "Go programming language (Nix-native installer)";

    version = mkOption {
      type = types.str;
      default = "latest";
      description = ''
        Go version to install.
        Use "latest" or specify a version like "1.23.4".

        Available versions: ${concatStringsSep ", " (attrNames goVersions)}
      '';
      example = "1.23.4";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ goPackage ];

    # Set GOPATH and GOBIN
    home.sessionVariables = {
      GOPATH = "$HOME/go";
      GOBIN = "$HOME/go/bin";
    };

    # Add Go bin directories to PATH
    home.sessionPath = [
      "$HOME/go/bin"
    ];
  };
}