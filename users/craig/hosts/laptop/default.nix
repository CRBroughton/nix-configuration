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
    (disko + "/laptop.nix")

    # Personal
    ../../flatpaks.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
  ];

  # Laptop power management
  services.power-profiles-daemon.enable = true;

  # Modules
  modules.gnome.enable = true;
  modules.tailscale.enable = true;
  modules.ssh.enable = true;
  modules.vpn.enable = true;
  modules.flatpak.enable = true;
  modules.yubikey.enable = true;
  modules.development.enable = true;
  modules.gaming.enable = true;
  modules.autoUpgrade.enable = true;
  modules.shell.enable = true;
  modules.git.enable = true;
  modules.terminal.enable = true;
  modules.media.enable = true;
  modules.editors.vscode.enable = true;
  modules.editors.neovim.enable = true;
  modules.editors.zed.enable = true;
  modules.browsers.zen.enable = true;

}
