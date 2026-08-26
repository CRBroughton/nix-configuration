# Terraria control API - lets Glance show connect IP/status and toggle
# terraria.service on/off. Tailscale-only, token-gated for the write endpoints.
{ config, lib, pkgs, ... }:
let
  cfg = config.modules.server.services.terraria-control;

  terraria-control = pkgs.buildGoModule {
    pname = "terraria-control";
    version = "0.1.0";
    src = ../../../services/terraria-control;
    vendorHash = null;
  };
in
{
  options.modules.server.services.terraria-control = {
    enable = lib.mkEnableOption "Terraria control API for the Glance dashboard";

    port = lib.mkOption {
      type = lib.types.port;
      default = 8088;
      description = "Port to listen on (Tailscale interface only).";
    };

    unit = lib.mkOption {
      type = lib.types.str;
      default = "terraria.service";
      description = "systemd unit this API is allowed to start/stop.";
    };

    lanInterface = lib.mkOption {
      type = lib.types.str;
      default = "eno1";
      description = "Interface to read the LAN IP from, for display.";
    };

    gamePort = lib.mkOption {
      type = lib.types.port;
      default = 7777;
      description = "Terraria game port, for display only.";
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.terraria_control_token = {
      file = ../../../secrets/nixos-server/terraria_control_token.age;
      owner = "terraria-control";
      group = "terraria-control";
      mode = "0400";
    };

    users.users.terraria-control = {
      isSystemUser = true;
      group = "terraria-control";
    };
    users.groups.terraria-control = { };

    # Scoped to exactly these two commands on this one unit — nothing else.
    security.sudo.extraRules = [
      {
        users = [ "terraria-control" ];
        commands = [
          {
            command = "${config.systemd.package}/bin/systemctl start ${cfg.unit}";
            options = [ "NOPASSWD" ];
          }
          {
            command = "${config.systemd.package}/bin/systemctl stop ${cfg.unit}";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];

    systemd.services.terraria-control = {
      description = "Terraria control API";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        User = "terraria-control";
        Group = "terraria-control";
        ExecStart = "${terraria-control}/bin/terraria-control";
        Restart = "always";
        Environment = [
          "PORT=${toString cfg.port}"
          "UNIT=${cfg.unit}"
          "LAN_INTERFACE=${cfg.lanInterface}"
          "GAME_PORT=${toString cfg.gamePort}"
          "TOKEN_FILE=${config.age.secrets.terraria_control_token.path}"
        ];

        NoNewPrivileges = false; # needs to invoke sudo
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
      };
    };

    # Tailnet-only — same pattern as harmonia.nix.
    networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ cfg.port ];
  };
}
