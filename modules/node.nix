{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.node-installer;

  # UPDATE THIS when new Node.js versions are released
  latestVersion = "25.2.1";
  latestLTSVersion = "24.11.1";

  # Function to create a Node.js derivation for a specific version
  buildNode = { version, sha256 }:
    pkgs.stdenv.mkDerivation {
      pname = "nodejs";
      inherit version;

      src = pkgs.fetchurl {
        url = "https://nodejs.org/dist/v${version}/node-v${version}-linux-x64.tar.gz";
        inherit sha256;
      };

      # No build needed, just extract and install
      dontBuild = true;
      dontUnpack = true;

      installPhase = ''
        runHook preInstall

        mkdir -p $out/lib/nodejs
        ${pkgs.gnutar}/bin/tar -xzf $src -C $out/lib/nodejs --strip-components=1

        # Symlink binaries to bin/
        mkdir -p $out/bin
        ln -s $out/lib/nodejs/bin/* $out/bin/

        runHook postInstall
      '';

      meta = with lib; {
        description = "Event-driven I/O framework for the V8 JavaScript engine";
        homepage = "https://nodejs.org/";
        license = licenses.mit;
        platforms = [ "x86_64-linux" ];
      };
    };

  # Predefined versions with their hashes
  # To get hash for a new version, run:
  # nix-prefetch-url https://nodejs.org/dist/v24.8.0/node-v24.8.0-linux-x64.tar.gz
  nodeVersions = {
    "25.2.1" = {
      version = "25.2.1";
      sha256 = "0h5rdckjsnnwhyjr6h904wyxd76qn1r2di6afybix8afhkffr510";
    };
    "24.11.1" = {
      version = "24.11.1";
      sha256 = "1jirhbai297c8v59km0in6hlm6f1slly68paid2hw87jr1fgz9aq";
    };
    "24.8.0" = {
      version = "24.8.0";
      sha256 = "0mg6j07ffp6n6h0lv7j31y61890aa010sn36crhw7d3qnh289xns";
    };
    "18.18.0" = {
      version = "18.18.0";
      sha256 = "18ikaln72b0nqqbdnwx2jlax2l5rfd1ixi9mki2mkmm5nfv65bla";
    };
  };

  selectedVersion =
    if cfg.version == "latest" then latestVersion
    else if cfg.version == "lts" then latestLTSVersion
    else cfg.version;

  # Filter out versions that are already represented by "latest" and "lts"
  otherVersions = filter (v: v != latestVersion && v != latestLTSVersion) (attrNames nodeVersions);

  versionConfig = nodeVersions.${selectedVersion} or (throw ''
    Node.js version ${selectedVersion} is not defined.
    Available versions: latest, lts${optionalString (otherVersions != []) ", ${concatStringsSep ", " otherVersions}"}

    To add a new version, get its hash by running:
      nix-prefetch-url https://nodejs.org/dist/v${selectedVersion}/node-v${selectedVersion}-linux-x64.tar.gz

    Then add it to nodeVersions in modules/node.nix
  '');

  nodePackage = buildNode versionConfig;
in
{
  options.programs.node-installer = {
    enable = mkEnableOption "Node.js runtime (Nix-native installer)";

    version = mkOption {
      type = types.str;
      default = "latest";
      description = ''
        Node.js version to install.
        - "latest": Latest version (${latestVersion})
        - "lts": Latest LTS version (${latestLTSVersion})
        - Or specify a version like "25.2.1", "24.11.1", "24.8.0", or "18.18.0"

        Available versions: latest, lts${optionalString (otherVersions != []) ", ${concatStringsSep ", " otherVersions}"}
      '';
      example = "lts";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ nodePackage ];

    # Add npm global bin directory to PATH
    home.sessionPath = [
      "$HOME/.npm-global/bin"
    ];

    # Set npm prefix for global packages
    home.sessionVariables = {
      NPM_CONFIG_PREFIX = "$HOME/.npm-global";
    };
  };
}
