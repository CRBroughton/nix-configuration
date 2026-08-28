# NixOS Server - Home server running Podman containers
{
  pkgs,
  lib,
  ...
}:

let
  # nixpkgs' terraria-server (1.4.5.6) lags the current client (1.4.5.8) —
  # fetch the matching version directly from terraria.org until nixpkgs
  # catches up. Shared by the main server and terraria-expert below.
  terrariaServerPkg = pkgs.terraria-server.overrideAttrs (old: {
    version = "1.4.5.8";
    src = pkgs.fetchurl {
      url = "https://terraria.org/api/download/pc-dedicated-server/terraria-server-1458.zip";
      sha256 = "sha256-9ROkrJeJ00r3Zika4hfJzX2UcuE3gqDisXUS9w16gzQ=";
    };
  });

  terrariaExpertDataDir = "/var/lib/terraria-expert";

  # nixpkgs' services.terraria is a singleton (one systemd unit, one
  # hardcoded user), so a second parallel server can't go through our
  # terraria-server module a second time — this reimplements the same
  # tmux-wrapped ExecStart/ExecStop pattern nixpkgs' terraria.nix uses,
  # scoped to its own user/dataDir/port.
  terrariaExpertTmuxCmd = "${lib.getExe pkgs.tmux} -S ${terrariaExpertDataDir}/terraria-expert.sock";

  terrariaExpertStopScript = pkgs.writeShellScript "terraria-expert-stop" ''
    if ! [ -d "/proc/$1" ]; then
      exit 0
    fi

    lastline=$(${terrariaExpertTmuxCmd} capture-pane -p | grep . | tail -n1)

    if [[ "$lastline" =~ ^'Choose World' ]]; then
      ${terrariaExpertTmuxCmd} kill-session
    else
      ${terrariaExpertTmuxCmd} send-keys Enter exit Enter
    fi

    tail --pid="$1" -f /dev/null
  '';
in
{
  imports = [
    ./hardware.nix
  ];

  age.identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  # Shared ts_authkey used by all nixos-server containers
  # owner/group craig so rootless podman-compose services can read it via env_file
  age.secrets.ts_authkey = {
    file = ../../secrets/nixos-server/ts_authkey.age;
    owner = "craig";
    group = "users";
    mode = "0400";
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
      7778 # Terraria (expert)
    ];
    allowedUDPPorts = [
      64738 # Mumble voice
      7778 # Terraria (expert)
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

  systemd.services.podman-create-homelab-network = {
    description = "Create podman homelab network";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.podman}/bin/podman network inspect homelab &>/dev/null || ${pkgs.podman}/bin/podman network create homelab'";
    };
  };

  # Modules
  modules.freshrss.enable = true;
  modules.syncserver.enable = true;
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
  modules.server.services.terraria-control.enable = true;
  services.trove = {
    enable = true;
    port = 8082;
    openFirewall = true;
  };
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

  services.terraria-server = {
    enable = true;
    difficulty = "expert";
    package = terrariaServerPkg;
  };

  # Second, parallel server: expert mode, generated with the same seed as
  # the main (classic) world so the terrain matches. See the
  # terrariaExpert* let-bindings above for why this isn't just a second
  # services.terraria-server block.
  users.users.terraria-expert = {
    isSystemUser = true;
    group = "terraria-expert";
    home = terrariaExpertDataDir;
    createHome = true;
  };
  users.groups.terraria-expert = { };

  systemd.services.terraria-expert = {
    description = "Terraria Expert Server Service";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    serviceConfig = {
      User = "terraria-expert";
      Group = "terraria-expert";
      Type = "forking";
      GuessMainPID = true;
      UMask = 7;
      ExecStart = "${terrariaExpertTmuxCmd} new -d ${lib.getExe terrariaServerPkg} "
        + ''-port 7778 -maxPlayers 255 -world "${terrariaExpertDataDir}/Worlds/world.wld" -autocreate 2 -difficulty 1 -seed "441227047"'';
      ExecStop = "${terrariaExpertStopScript} $MAINPID";
    };
  };
}
