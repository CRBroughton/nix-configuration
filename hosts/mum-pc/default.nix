{ disko, ... }:

{
  imports = [
    ../../users/mum/vm-testing.nix
    ./hardware.nix
    (disko + "/mum-pc.nix")
  ];

  networking.networkmanager.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Decrypt RDP password secret for GNOME Remote Desktop
  age.secrets.mum-rdp-password = {
    file = ../../secrets/mum-rdp-password.age;
    owner = "mum";
  };

  # Allow GNOME Remote Desktop (RDP) only on the Tailscale interface
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 3389 ];

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
