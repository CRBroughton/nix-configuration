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
      description = "Loopback-only port the API listens on.";
    };

    servePort = lib.mkOption {
      type = lib.types.port;
      default = 8443;
      description = ''
        HTTPS port `tailscale serve` exposes on the tailnet, proxying to
        the loopback `port` above. Needed because Glance is served over
        HTTPS — an iframe/fetch pointed at plain http gets blocked as
        mixed content by the browser.
      '';
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
            # Must match the exact path terraria-control's SYSTEMCTL_BIN
            # invokes (see main.go) — sudo matches commands by literal path,
            # and this stable symlink is what the app is pinned to.
            command = "/run/current-system/sw/bin/systemctl start ${cfg.unit}";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/systemctl stop ${cfg.unit}";
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

    # Terminates HTTPS on the tailnet (valid cert for the MagicDNS name) and
    # proxies to the loopback-only API — not exposed via the host firewall
    # at all, only reachable through tailscaled's own serve mechanism.
    systemd.services.terraria-control-serve = {
      description = "Tailscale HTTPS proxy for terraria-control";
      wantedBy = [ "multi-user.target" ];
      after = [ "tailscaled.service" "terraria-control.service" ];
      wants = [ "tailscaled.service" "terraria-control.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.tailscale}/bin/tailscale serve --bg --https=${toString cfg.servePort} http://127.0.0.1:${toString cfg.port}";
        ExecStop = "${pkgs.tailscale}/bin/tailscale serve --https=${toString cfg.servePort} off";
      };
    };
  };
}
