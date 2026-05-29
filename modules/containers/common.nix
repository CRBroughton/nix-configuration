_: {
  services.prometheus.exporters.node = {
    enable = true;
    port = 9100;
  };
  networking.firewall.allowedTCPPorts = [ 9100 ];
  system.stateVersion = "26.05";
}
