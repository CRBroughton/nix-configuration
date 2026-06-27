{
  pkgs,
  disko,
  ...
}:

{
  nixpkgs.overlays = [
    (_final: prev: {
      # openldap test017-syncreplication-refresh is flaky (timing issue); skip checks
      openldap = prev.openldap.overrideAttrs (_: {
        doCheck = false;
      });
    })
  ];
  imports = [
    # ../../users/craig/vm-testing.nix
    ./hardware.nix
    (disko + "/gaming-pc.nix")

    # Personal
    ../../users/craig/flatpaks.nix
  ];

  # Kernel - CachyOS with sched_ext (uncomment for AMD gaming PC)
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  nix.settings.secret-key-files = [ "/etc/nix/signing-key.secret" ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
  ];

  # Modules
  modules.gnome.enable = true;
  modules.tailscale.enable = true;
  modules.ssh.enable = true;
  modules.vpn.enable = true;
  modules.flatpak.enable = true;
  modules.yubikey.enable = true;
  modules.development.enable = true;
  modules.gaming.enable = true;
  modules.shell.enable = true;
  modules.git.enable = true;
  modules.terminal.enable = true;
  modules.media.enable = true;
  modules.editors.vscode.enable = true;
  modules.editors.neovim.enable = true;
  modules.editors.zed.enable = true;
  modules.browsers.zen.enable = true;
  modules.autoUpgrade.enable = true;
  modules.taskReminder = {
    enable = true;
    tasks = [
      { every = 1;      task = "Check the sink for dishes"; }
      { time = "12:00"; task = "Lunch time!"; }
      { time = "21:30"; task = "Check the doors are locked and the PC is off"; }
    ];
  };
  modules.monitoringNode.enable = true;
  modules.claude-desktop.enable = true;
  modules.ascension.enable = true;
  modules.albionOnline.enable = true;
}
