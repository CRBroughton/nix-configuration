# VM testing settings
# Import this in host configs for local VM testing
# Remove this import when deploying to real hardware
_:

{
  # Auto-login for convenience during VM testing
  users.users.craig.initialPassword = "test";
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "craig";

  # VM variant settings (only affects nixos-rebuild build-vm)
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
