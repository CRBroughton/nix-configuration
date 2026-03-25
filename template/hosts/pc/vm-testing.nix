# VM testing settings - remove this import in default.nix when deploying to real hardware
_:

{
  # Password for VM login (user: demo, password: test)
  users.users.demo.initialPassword = "test";

  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      qemu.options = [ "-enable-kvm" ];
    };
  };
}
