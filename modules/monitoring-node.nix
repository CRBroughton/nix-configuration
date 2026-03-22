# Monitoring Node - Prometheus node_exporter + NixOS upgrade status reporting
# Enable on any machine that should report to the fleet dashboard
{
  config,
  lib,
  pkgs,
  hostname,
  ...
}:

let
  cfg = config.modules.monitoringNode;
  textfileDir = "/var/lib/prometheus-node-exporter/textfile";
in
{
  options.modules.monitoringNode = {
    enable = lib.mkEnableOption "Prometheus node exporter and NixOS upgrade reporting";

    promtail = {
      enable = lib.mkEnableOption "Promtail log shipping to Loki" // { default = true; };
      lokiUrl = lib.mkOption {
        type = lib.types.str;
        default = "https://loki.tail538465.ts.net/loki/api/v1/push";
        description = "Loki push endpoint URL";
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        services.prometheus.exporters.node = {
          enable = true;
          port = 9100;
          enabledCollectors = [
            "systemd"
            "textfile"
          ];
          extraFlags = [ "--collector.textfile.directory=${textfileDir}" ];
        };

        # Create the textfile directory for the upgrade reporter to write into
        systemd.tmpfiles.rules = [
          "d ${textfileDir} 0755 root root -"
        ];

        # Only expose node_exporter on the Tailscale interface
        networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 9100 ];

        # Write a Prometheus textfile after each NixOS upgrade completes (success or failure)
        systemd.services."nixos-upgrade-report" = {
          description = "Report NixOS upgrade result to Prometheus textfile";
          after = [ "nixos-upgrade.service" ];
          wantedBy = [ "nixos-upgrade.service" ];
          serviceConfig = {
            Type = "oneshot";
            ExecStart = pkgs.writeShellScript "nixos-upgrade-report" ''
                        OUTFILE="${textfileDir}/nixos_upgrade.prom"

                        RESULT=$(${pkgs.systemd}/bin/systemctl show nixos-upgrade.service --property=Result --value)
                        SUCCESS=0
                        [ "$RESULT" = "success" ] && SUCCESS=1

                        GEN=$(basename "$(readlink /nix/var/nix/profiles/system)" | grep -o '[0-9]*' | head -1)
                        TS=$(date +%s)

                        START=$(${pkgs.systemd}/bin/systemctl show nixos-upgrade.service --property=ExecMainStartTimestampMonotonic --value)
                        EXIT_TS=$(${pkgs.systemd}/bin/systemctl show nixos-upgrade.service --property=ExecMainExitTimestampMonotonic --value)
                        DURATION_SEC=$(( (EXIT_TS - START) / 1000000 ))

                        cat > "$OUTFILE" <<EOF
              # HELP nixos_upgrade_success 1 if last upgrade succeeded, 0 if failed
              # TYPE nixos_upgrade_success gauge
              nixos_upgrade_success{host="${hostname}"} $SUCCESS
              # HELP nixos_upgrade_generation Current NixOS generation number
              # TYPE nixos_upgrade_generation gauge
              nixos_upgrade_generation{host="${hostname}"} $GEN
              # HELP nixos_upgrade_timestamp_seconds Unix timestamp of last upgrade attempt
              # TYPE nixos_upgrade_timestamp_seconds gauge
              nixos_upgrade_timestamp_seconds{host="${hostname}"} $TS
              # HELP nixos_upgrade_duration_seconds Duration of last upgrade in seconds
              # TYPE nixos_upgrade_duration_seconds gauge
              nixos_upgrade_duration_seconds{host="${hostname}"} $DURATION_SEC
              EOF
            '';
          };
        };
      }

      # Ship systemd journal logs to Loki via Promtail
      (lib.mkIf cfg.promtail.enable {
        services.promtail = {
          enable = true;
          configuration = {
            server = {
              http_listen_port = 9080;
              grpc_listen_port = 0;
            };
            clients = [
              { url = cfg.promtail.lokiUrl; }
            ];
            scrape_configs = [
              {
                job_name = "systemd-journal";
                journal = {
                  max_age = "12h";
                  labels = {
                    job = "systemd-journal";
                    host = hostname;
                  };
                };
                relabel_configs = [
                  {
                    source_labels = [ "__journal__systemd_unit" ];
                    target_label = "unit";
                  }
                ];
              }
            ];
          };
        };
      })
    ]
  );
}
