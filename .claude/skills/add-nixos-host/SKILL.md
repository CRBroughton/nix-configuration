---
name: add-nixos-host
description: Add a new NixOS host to this flake-based nix-configuration repo. Use when the user wants to add a new machine, user, or host configuration.
---

You are helping add a new NixOS host to this repository. Follow these steps precisely.

## Gather information

Ask the user for (if not already provided):
1. **hostname** – the machine name (e.g. `mums-laptop`)
2. **username** – the system user (e.g. `mum`, `craig`)
3. **desktop** – `gnome`, `kde`, or headless/server
4. **purpose** – desktop, gaming, server, or minimal
5. **new user?** – does `users/<username>/` already exist?

---

## Directory structure to create

```
users/<username>/           ← skip if user already exists
  common.nix
  default.nix
  vm-testing.nix            ← optional, for VM testing

users/<username>/hosts/<hostname>/
  default.nix
  hardware.nix
```

---

## File templates

### `users/<username>/common.nix`
System-level user config. Adapt groups to the user's needs.

```nix
{ pkgs, ... }:
{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.<username> = {
    isNormalUser = true;
    description = "<Full Name>";
    extraGroups = [ "networkmanager" "audio" "video" ];
    initialPassword = "changeme";
    shell = pkgs.bash;   # use pkgs.fish for power users
  };
}
```

Add `"wheel"` to `extraGroups` if admin. Add `"libvirtd"` for virtualisation.

---

### `users/<username>/default.nix`
Home-manager identity. Keep minimal unless the user has personal dotfiles.

```nix
_:
{
  home.homeDirectory = "/home/<username>";
}
```

---

### `users/<username>/vm-testing.nix` (optional)
Useful for local VM smoke-testing before deploying to real hardware.

```nix
{ pkgs, lib, ... }:
{
  services.displayManager.autoLogin = {
    enable = true;
    user = "<username>";
  };

  virtualisation = {
    memorySize = 4096;
    cores = 4;
    diskSize = 20480;
    qemu.options = [ "-enable-kvm" ];
  };
}
```

---

### `users/<username>/hosts/<hostname>/default.nix`
Main NixOS host config. Choose modules appropriate to the machine's purpose.

```nix
{ ... }:
{
  imports = [
    ../../common.nix
    # ../../vm-testing.nix   # uncomment only for VM testing
    ./hardware.nix
  ];

  networking.hostName = "<hostname>";
  networking.networkmanager.enable = true;

  # Enable modules — remove or add as needed:
  modules.gnome.enable = true;       # or modules.kde.enable
  modules.shell.enable = true;
  modules.flatpak.enable = true;
  # modules.gaming.enable = true;
  # modules.development.enable = true;
  # modules.tailscale.enable = true;
  # modules.ssh.enable = true;
}
```

Available modules (dot-notation, all auto-imported via import-tree):
- Desktop: `modules.gnome`, `modules.kde`
- User env: `modules.shell`, `modules.terminal`, `modules.git`, `modules.media`
- Apps: `modules.gaming`, `modules.flatpak`, `modules.editors.vscode`, `modules.browsers.zen`
- System: `modules.development`, `modules.tailscale`, `modules.ssh`, `modules.vpn`, `modules.autoUpgrade`
- Server: `modules.server.ssh`, `modules.server.podman`, `modules.server.arion`, `modules.server.restic`

---

### `users/<username>/hosts/<hostname>/hardware.nix`
**On real hardware**, generate with:
```bash
nixos-generate-config --show-hardware-config
```

For now, create a placeholder:
```nix
{ ... }:
{
  # Hardware config — replace with output of:
  # nixos-generate-config --show-hardware-config
}
```

---

## Register the host in `flake.nix`

Add to the `nixosConfigurations` attrset:

```nix
<hostname> = myLib.mkHost {
  hostname = "<hostname>";
  user = "<username>";
  inherit stateVersion;
  extraModules = [ inputs.nixos-hardware.nixosModules.common-pc-laptop ];  # optional
};
```

Find the block that contains the other `mkHost` calls and add the new entry alongside them.

---

## After creating files

1. Stage new files — **git add is required before nix can see them**:
   ```bash
   git add users/<username>/
   ```
2. Check the config:
   ```bash
   nix flake check
   ```
3. Test in a VM (if vm-testing.nix was added and justfile has a matching target):
   ```bash
   just vm-<hostname>
   ```

---

## Notes

- Do **not** manually import modules — `import-tree` auto-loads everything in `modules/`
- The `user` variable (from `mkHost`) is automatically available in all module configs
- `stateVersion` is inherited from `flake.nix` — do not hardcode it in host files
- Remove the `vm-testing.nix` import before deploying to real hardware
