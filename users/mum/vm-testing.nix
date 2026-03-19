# VM testing settings
# Import this in host configs for local VM testing
# Remove this import when deploying to real hardware
_:

{
  users.users.mum.initialPassword = "test";
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "mum";

  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
