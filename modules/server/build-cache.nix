# Build all host configs after upgrade so Harmonia can serve them as a binary cache
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.server.buildCache;

  buildScript = pkgs.writeShellScript "nix-build-cache" ''
    set -uo pipefail

    GCROOT_DIR="/nix/var/nix/gcroots/nix-build-cache"
    mkdir -p "$GCROOT_DIR"

    for host in ${lib.escapeShellArgs cfg.hosts}; do
      echo "Building $host..."
      ${pkgs.nix}/bin/nix build "${cfg.flakeRef}#nixosConfigurations.''${host}.config.system.build.toplevel" \
        --out-link "$GCROOT_DIR/''${host}" \
        --log-format raw \
        2>&1 || echo "WARN: failed to build $host, continuing"
    done
  '';
in
{
  options.modules.server.buildCache = {
    enable = lib.mkEnableOption "pre-build all host configurations so Harmonia can serve them";
    flakeRef = lib.mkOption {
      type = lib.types.str;
      description = "Flake ref to build from (e.g. github:user/repo)";
    };
    hosts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "NixOS configuration names to pre-build (matching flake outputs)";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services."nix-build-cache" = {
      description = "Pre-build NixOS host configurations for binary cache";
      # Run after the git pull so we're building the latest config
      after = [
        "network-online.target"
        "custom-nixos-upgrade-git-pull.service"
        "nixos-upgrade.service"
      ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = buildScript;
        TimeoutStartSec = "8h";
        StandardOutput = "journal";
        StandardError = "journal";
      };
      # Activate alongside the daily upgrade
      wantedBy = [ "nixos-upgrade.service" ];
    };
  };
}
