# NixOS Server - Home server running Podman containers
{ config, pkgs, modules, ... }:

{
  imports = [
    ../../common.nix
    ./hardware.nix

    # Server modules
    (modules + "/server/ssh.nix")
    (modules + "/server/tailscale.nix")
    (modules + "/server/podman.nix")
    (modules + "/server/restic.nix")
    (modules + "/server/auto-upgrade.nix")
    (modules + "/server/container-auto-update.nix")
    (modules + "/nix.nix")

    # Arion (declarative containers)
    (modules + "/server/arion.nix")
    (modules + "/server/services/freshrss.nix")
  ];

  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable aarch64 emulation for building Pi images
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # Network
  networking.networkmanager.enable = true;

  # Add podman group to user
  users.users.craig.extraGroups = [ "podman" ];

  # UID/GID mapping for rootless containers
  users.users.craig.subUidRanges = [{ startUid = 100000; count = 65536; }];
  users.users.craig.subGidRanges = [{ startGid = 100000; count = 65536; }];

  # Shell
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Firewall - open ports for services
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22    # SSH
      53    # Adguard DNS
      3000  # Adguard web UI
      3923  # Copyparty
      4000  # Searxng
      5222  # XMPP (Prosody)
      5280  # XMPP HTTP (BOSH/WebSocket)
      6697  # IRC TLS
      8080  # Open WebUI
      8083  # Calibre
      8787  # FreshRSS
      8888  # Glance
      9000  # TheLounge
      9090  # Linkding
      9925  # Mealie
      64738 # Mumble
    ];
    allowedUDPPorts = [
      53    # Adguard DNS
      64738 # Mumble voice
    ];
  };

  # Enable lingering for user (keeps user services running)
  systemd.services."user-linger-craig" = {
    description = "Enable lingering for craig";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/loginctl enable-linger craig";
      RemainAfterExit = true;
    };
  };

  # System packages
  environment.systemPackages = with pkgs; [
    openssh
    openssl
    lazygit
    btop
  ];

  system.stateVersion = "25.11";
}
