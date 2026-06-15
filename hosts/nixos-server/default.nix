# NixOS Server - Home server running Podman containers
{
  pkgs,
  ...
}:

{
  imports = [
    ./hardware.nix
  ];

  age.identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  # Shared ts_authkey used by all nixos-server containers
  age.secrets.ts_authkey = {
    file = ../../secrets/nixos-server/ts_authkey.age;
  };

  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable aarch64 emulation for building Pi images
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # Network
  networking.networkmanager.enable = true;

  # NAT for nixos-containers with privateNetwork = true
  networking.nat = {
    enable = true;
    internalInterfaces = [ "ve-+" ];
    externalInterface = "eno1";
  };

  # Prevent NetworkManager from managing container veth interfaces
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];

  # Add podman group to user
  users.users.craig.extraGroups = [ "podman" ];

  # UID/GID mapping for rootless containers
  users.users.craig.subUidRanges = [
    {
      startUid = 100000;
      count = 65536;
    }
  ];
  users.users.craig.subGidRanges = [
    {
      startGid = 100000;
      count = 65536;
    }
  ];

  # Shell
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Firewall - open ports for services
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22 # SSH
      3923 # Copyparty
      4000 # Searxng
      5222 # XMPP (Prosody)
      5280 # XMPP HTTP (BOSH/WebSocket)
      6697 # IRC TLS
      8080 # Open WebUI
      8083 # Calibre
      8888 # Glance
      9000 # TheLounge
      9090 # Linkding
      9925 # Mealie
      64738 # Mumble
    ];
    allowedUDPPorts = [
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
    systemctl-tui
  ];

  # Modules
  modules.freshrss.enable = true;
  modules.shell.enable = true;
  modules.git.enable = true;
  modules.server.ssh.enable = true;
  modules.server.tailscale.enable = true;
  modules.server.podman.enable = true;
  modules.server.restic.enable = true;
  modules.server.autoUpgrade.enable = true;
  modules.monitoringNode.enable = true;
  modules.server.containerAutoUpdate.enable = true;
  modules.server.services.harmonia.enable = true;
  modules.server.buildCache = {
    enable = true;
    flakeRef = "github:CRBroughton/nix-configuration";
    hosts = [
      "gaming-pc"
      "laptop"
      "brighton-pc"
      "mum-pc"
      "mums-laptop"
      "moons-pc"
    ];
  };
}
