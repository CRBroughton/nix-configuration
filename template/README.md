# NixOS Configuration Template

A minimal NixOS configuration template using auto-imported modules with opt-in dot-notation enables. No desktop environment, no custom shell, no opinionated defaults — just the structure to build from.

## What you get out of the box

- A bootable system (systemd-boot, EFI)
- NetworkManager for networking
- A single user with `wheel` (sudo) access
- `git`, `vim`, `curl`, `wget` as basic system packages
- Home-manager wired up and ready for user-level config
- Auto-imported modules — add a file, enable it with dot-notation
- A VM target so you can test changes before touching real hardware
- A `justfile` with common commands (`switch`, `update`, `vm`, `rollback`, `clean`)

What it intentionally leaves out:

- No desktop environment
- No custom shell (defaults to bash)
- No theming, fonts, or visual config
- No developer tools, gaming, or media packages
- No additional services

Everything beyond the basics is opt-in — you add what you need, nothing more.

## Why this approach?

### Auto-imported modules

All `.nix` files under `modules/` are automatically imported as NixOS modules via [`import-tree`](https://github.com/vic/import-tree). There are no manual import paths in host configs — just enable what you need:

```nix
# users/yourname/hosts/yourhostname/default.nix
desktops.gnome.enable = true;
shell.enable = true;
```

The same applies to home-manager modules under `modules/_home/` — they're auto-imported via `sharedModules` and available to every user:

```nix
# users/yourname/default.nix
shell.enable = true;
git.enable = true;
```

The `_home/` prefix tells import-tree to skip those files during the NixOS module sweep (they're home-manager modules, not NixOS modules).

### Module pattern

Every module follows the same shape:

```nix
{ config, lib, pkgs, ... }:

let cfg = config.myFeature; in
{
  options.myFeature = {
    enable = lib.mkEnableOption "description";
  };

  config = lib.mkIf cfg.enable {
    # config only applied when enabled
  };
}
```

This means all modules are always available but off by default. Enable only what you need per host or user. The LSP (`nixd`) shows the description from `mkEnableOption` as hover documentation.

### Flake-based

All inputs are pinned in `flake.lock`. Fully reproducible — rebuild the exact same system anywhere, any time.

### User/host separation

Config is split by user and machine under `users/<name>/hosts/<hostname>/`:

- User-level config (shell, editor, packages) lives under `users/yourname/`
- Machine-level config (hardware, services) lives under `users/yourname/hosts/yourhostname/`
- Shared user config automatically applies to every machine that user has

### Home-manager included

[Home-manager](https://github.com/nix-community/home-manager) is wired up from the start. Home-manager modules in `modules/_home/` are auto-loaded into every user's context.

### VM-first workflow

The `vm-testing.nix` file and `just vm` command let you test changes in a VM before applying to real hardware.

For a fuller example — multiple machines, desktop environments, server config, gaming — see the [full configuration](https://github.com/crbroughton/nix-configuration) this template is based on.

## Structure

```
template/
├── flake.nix                          # Entry point, import-tree sweep
├── justfile                           # Common commands
├── lib/
│   └── default.nix                    # mkHost helper, homeModules wiring
├── modules/
│   ├── desktop/
│   │   └── gnome.nix                  # desktops.gnome.enable
│   └── _home/                         # Home-manager modules (skipped by NixOS sweep)
│       └── shell.nix                  # shell.enable
└── users/
    └── demo/                          # Your user
        ├── default.nix                # Home-manager config (dot-notation enables)
        └── hosts/
            └── pc/                    # Your machine
                ├── default.nix        # Host config (dot-notation enables)
                ├── hardware.nix       # Hardware config (generated)
                └── vm-testing.nix     # VM settings (remove for real hardware)
```

## Getting Started

### 1. Rename placeholders

```bash
mv users/demo users/yourname
mv users/yourname/hosts/pc users/yourname/hosts/yourhostname
```

Then update:

| File | What to change |
|------|----------------|
| `flake.nix` | `hostname = "pc"` and `user = "demo"` |
| `users/yourname/hosts/yourhostname/default.nix` | `users.users.demo` → `users.users.yourname` |
| `users/yourname/hosts/yourhostname/vm-testing.nix` | `users.users.demo` → `users.users.yourname` |
| `justfile` | `hostname` in the `vm` command |

### 2. Generate hardware config

On your target machine (or NixOS installer):

```bash
nixos-generate-config --show-hardware-config > users/yourname/hosts/yourhostname/hardware.nix
```

### 3. Test in a VM first

```bash
just vm
```

Login with user `demo` (or whatever you renamed it to), password `test`.

### 4. Install on real hardware

```bash
nix-shell -p git just
git clone <your-repo> /mnt/etc/nixos
cd /mnt/etc/nixos
sudo nixos-install --flake .#yourhostname
```

Remove the `vm-testing.nix` import before deploying to real hardware.

### 5. Apply changes

```bash
just switch
```

---

## Expanding the Configuration

### Adding a desktop environment

Uncomment in `users/yourname/hosts/yourhostname/default.nix`:

```nix
desktops.gnome.enable = true;
```

That's it. The module is already auto-imported.

### Adding a NixOS module

Create a file in `modules/`:

```nix
# modules/my-feature.nix
{ config, lib, pkgs, ... }:

let cfg = config.myFeature; in
{
  options.myFeature = {
    enable = lib.mkEnableOption "my feature description";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.htop ];
  };
}
```

Then `git add` it and enable in any host:

```nix
myFeature.enable = true;
```

### Adding a home-manager module

Create a file in `modules/_home/`:

```nix
# modules/_home/my-tool.nix
{ config, lib, pkgs, ... }:

let cfg = config.myTool; in
{
  options.myTool = {
    enable = lib.mkEnableOption "my tool description";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.htop ];
  };
}
```

Then `git add` it and enable in any user's `default.nix`:

```nix
myTool.enable = true;
```

### Enabling the shell module

The template includes an example home-manager module at `modules/_home/shell.nix`. Enable it in `users/yourname/default.nix`:

```nix
shell.enable = true;
```

This gives you fish shell, starship prompt, and common CLI tools.

### Adding another machine

```
users/yourname/hosts/
├── yourhostname/      # existing
└── new-machine/       # new
    ├── default.nix
    ├── hardware.nix
    └── vm-testing.nix
```

Add it to `flake.nix`:

```nix
new-machine = myLib.mkHost {
  hostname = "new-machine";
  user = "yourname";
  extraModules = [ modules ];
};
```

### Adding another user

```
users/
├── yourname/          # existing
└── other-user/        # new
    ├── default.nix
    ├── common.nix
    └── hosts/
        └── their-machine/
```

Add it to `flake.nix`:

```nix
their-machine = myLib.mkHost {
  hostname = "their-machine";
  user = "other-user";
  extraModules = [ modules ];
};
```

Their `default.nix` gets the same auto-imported home-manager modules — they just enable what they need.

---

## Dev Shell

```bash
nix develop
```

| Tool | Purpose |
|------|---------|
| `nixfmt` | Format Nix files |
| `statix` | Lint for anti-patterns |
| `deadnix` | Find unused code |
| `nixd` / `nil` | Nix language servers |
| `nix-format` | Format all files in one command |

## Common Commands

| Command | Description |
|---------|-------------|
| `just switch` | Apply configuration changes |
| `just switch-verbose` | Apply with full error output |
| `just update` | Update flake inputs |
| `just update-all` | Update + apply in one step |
| `just vm` | Build and run in a VM |
| `just check` | Check for errors without building |
| `just rollback` | Revert to previous generation |
| `just clean` | Remove old generations (30d+) |
| `just maintenance` | Clean + optimise store |

## VM Notes

The `vm-testing.nix` file configures the VM with 4GB RAM, 4 cores, 20GB disk, and KVM acceleration. Password is set to `test` for easy login.

Remove the `vm-testing.nix` import before installing on real hardware.
