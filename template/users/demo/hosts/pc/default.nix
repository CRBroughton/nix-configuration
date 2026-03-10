# Host configuration for hostname
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./vm-testing.nix # Remove this line when deploying to real hardware
  ];

  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Network
  networking.networkmanager.enable = true;

  # User
  users.users.demo = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    shell = pkgs.bash;
  };

  # Shell
  programs.fish.enable = false;

  # Sudo without password (optional, remove if you prefer password prompts)
  security.sudo.wheelNeedsPassword = true;

  # Basic packages
  environment.systemPackages = with pkgs; [
    git
    vim
    curl
    wget
  ];

  system.stateVersion = "25.11";
}
