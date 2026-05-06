{ disko, ... }:

{
  imports = [
    ../../users/mum/vm-testing.nix
    ./hardware.nix
    (disko + "/mum-pc.nix")
  ];

  networking.networkmanager.enable = true;
  security.sudo.wheelNeedsPassword = false;

  services.wayvnc = {
    enable = true;
    host = "0.0.0.0";
    port = 5900;
    extraConfig = ''
      enable_auth=false
    '';
  };

  # Allow wayvnc only from Tailscale CGNAT range
  networking.firewall.extraInputRules = ''
    ip saddr 100.64.0.0/10 tcp dport 5900 accept
  '';

  modules.ssh.enable = true;
  modules.gnome.enable = true;
  modules.flatpak.enable = true;
  modules.gaming.enable = true;
  modules.monitoringNode.enable = true;
  modules.tailscale.enable = true;
  modules.autoUpgrade = {
    enable = true;
    dates = "*:0/15"; # every 15 minutes — picks up pushed changes quickly
  };
}
