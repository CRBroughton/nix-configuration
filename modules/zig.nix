{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.zig-installer;
  updateChecker = import ./update-checker.nix { inherit pkgs lib; };

  # UPDATE THESE when new Zig versions are released
  latestStableVersion = "0.15.2";
  latestDevVersion = "0.16.0-dev";

  # Function to create a Zig derivation for a specific version
  buildZig = { version, sha256, url ? null }:
    let
      downloadUrl = if url != null
        then url
        else "https://ziglang.org/download/${version}/zig-x86_64-linux-${version}.tar.xz";
    in
    pkgs.stdenv.mkDerivation {
      pname = "zig";
      inherit version;

      src = pkgs.fetchurl {
        url = downloadUrl;
        inherit sha256;
      };

      dontBuild = true;
      dontUnpack = true;

      installPhase = ''
        runHook preInstall

        mkdir -p $out/lib/zig
        ${pkgs.gnutar}/bin/tar -xJf $src -C /tmp
        cp -r /tmp/zig-x86_64-linux-${version}/* $out/lib/zig/
        rm -rf /tmp/zig-x86_64-linux-${version}

        # Symlink binaries to bin/
        mkdir -p $out/bin
        ln -s $out/lib/zig/zig $out/bin/zig

        runHook postInstall
      '';

      meta = with lib; {
        description = "General-purpose programming language and toolchain for maintaining robust, optimal, and reusable software";
        homepage = "https://ziglang.org/";
        license = licenses.mit;
        platforms = [ "x86_64-linux" ];
      };
    };

  # Predefined versions with their hashes
  # For stable versions, run:
  # nix-prefetch-url https://ziglang.org/download/0.15.2/zig-x86_64-linux-0.15.2.tar.xz
  # For dev builds, run:
  # nix-prefetch-url https://ziglang.org/builds/zig-x86_64-linux-0.16.0-dev.1344+bc589c271.tar.xz
  zigVersions = {
    "0.16.0-dev" = {
      version = "0.16.0-dev.1344+bc589c271";
      sha256 = "00faxr76vsfr9wq47wlpzy6w8xc7qa2hf49j10d6vnpv0jbwknhr";
      url = "https://ziglang.org/builds/zig-x86_64-linux-0.16.0-dev.1344+bc589c271.tar.xz";
    };
    "0.15.2" = {
      version = "0.15.2";
      sha256 = "0f9jz61fpjgc7bgfnl2hwm4ilgx68jn1s2wjnpjpd8ix307jgah2";
    };
    "0.14.1" = {
      version = "0.14.1";
      sha256 = "0v3iqjal325ab79l1yprgp7v533sh1fdkmvc9a9q3hqnmz4fxbi4";
    };
    "0.13.0" = {
      version = "0.13.0";
      sha256 = "1sih28hni53cnqbd3ap42vx12p4isrzwzi3vnwr81i5w3vk14lyl";
    };
    "0.12.1" = {
      version = "0.12.1";
      sha256 = "0hx4lipwq11i3sfgj5qj8jicbqqikcz8b3q0cdx2knf24nbzqq48";
    };
  };

  selectedVersion = if cfg.version == "latest"
    then latestStableVersion
    else if cfg.version == "dev"
    then latestDevVersion
    else cfg.version;

  versionConfig = zigVersions.${selectedVersion} or (throw ''
    Zig version ${selectedVersion} is not defined.
    Available versions: ${concatStringsSep ", " (attrNames zigVersions)}

    To add a new version, get its hash by running:
      nix-prefetch-url https://ziglang.org/download/${selectedVersion}/zig-linux-x86_64-${selectedVersion}.tar.xz

    Then add it to zigVersions in modules/zig.nix
  '');

  zigPackage = buildZig versionConfig;
in
{
  options.programs.zig-installer = {
    enable = mkEnableOption "Zig programming language (Nix-native installer)";

    version = mkOption {
      type = types.str;
      default = "latest";
      description = ''
        Zig version to install.
        - "latest" = Latest stable release (${zigVersions.${latestStableVersion}.version})
        - "dev" = Latest development build (${zigVersions.${latestDevVersion}.version})
        - Specific version like "0.15.2", "0.14.1", etc.

        Available versions: ${concatStringsSep ", " (attrNames zigVersions)}
      '';
      example = "0.15.2";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      zigPackage
      pkgs.zls
     ];

    # Zig doesn't require special environment variables like Go
    # The binary is self-contained

    # Check for updates on activation
    home.activation.checkZigUpdates = updateChecker.mkUpdateChecker {
      name = "Zig";
      currentVersion = selectedVersion;
      fetchLatest = ''
        ${pkgs.curl}/bin/curl -sL https://ziglang.org/download/index.json 2>/dev/null | ${pkgs.gnugrep}/bin/grep -oP '"[0-9]+\.[0-9]+\.[0-9]+"' | tr -d '"' | sort -V | tail -n1
      '';
      updateInstructions = ''
        echo "To update, run:"
        echo "  nix-prefetch-url https://ziglang.org/download/\$LATEST_VERSION/zig-linux-x86_64-\$LATEST_VERSION.tar.xz"
        echo ""
        echo "Then add to modules/zig.nix zigVersions:"
        echo "  \"\$LATEST_VERSION\" = {"
        echo "    version = \"\$LATEST_VERSION\";"
        echo "    sha256 = \"<hash-from-above>\";"
        echo "  };"
        echo ""
        echo "And update: latestStableVersion = \"\$LATEST_VERSION\";"
      '';
    };
  };
}
