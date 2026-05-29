let
  # SSH host keys (used by agenix to decrypt secrets on each machine)
  mum-pc = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7sVItvLyZ2OW/eb15woyIcwxVVM5Vr8BTA8t4TOdEE root@mum-pc";

  nixos-server = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnySPMy1TWevRfGnUqDORCrRfJ5n8IsB+xvb3U/6kvN root@nixos-server";

  # Craig's personal key (for editing secrets)
  craig = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk";
in
{
  "mum-rdp-password.age".publicKeys = [
    mum-pc
    craig
  ];

  "nixos-server/freshrss_password.age".publicKeys = [
    nixos-server
    craig
  ];

  # Shared across all nixos-server containers
  "nixos-server/ts_authkey.age".publicKeys = [
    nixos-server
    craig
  ];
}
