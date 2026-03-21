{
  pkgs,
  disko,
  ...
}:

{
  imports = [
    ../../common.nix
    #../../vm-testing.nix
    ./hardware.nix
    (disko + "/gaming-pc.nix")

    # Personal
    ../../flatpaks.nix
  ];

  # Kernel - CachyOS with sched_ext (uncomment for AMD gaming PC)
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  # I225-V NIC drops connection after sleep - disable Ultra Low Power mode
  boot.extraModprobeConfig = "options igc enable_ulp=0";
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
  modules.monitoringNode.enable = true;
}
