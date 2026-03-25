# NixOS Configuration Template

A minimal NixOS configuration template using [`nixos-lib`](../nixos-lib) — a reusable library that handles all the flake wiring. No desktop environment, no custom shell, no opinionated defaults — just the structure to build from.

## What you get out of the box

- A bootable system (systemd-boot, EFI)
- NetworkManager for networking
- A single user with `wheel` (sudo) access
- `git`, `vim`, `curl`, `wget` as basic system packages
- Home-manager wired up and ready for user-level config
- Auto-imported modules — add a file, enable it with dot-notation
- A VM target so you can test changes before touching real hardware
- A `justfile` with common commands (`switch`, `update`, `vm`, `rollback`, `clean`)
- Dev shell with formatting and linting tools (`nix develop`)

What it intentionally leaves out:

- No desktop environment
- No custom shell (defaults to bash)
- No theming, fonts, or visual config
- No developer tools, gaming, or media packages
- No additional services

Everything beyond the basics is opt-in — you add what you need, nothing more.

## Structure

```
template/
├── flake.nix                          # Entire flake — one input, one call
├── justfile                           # Common commands
├── modules/
│   ├── desktop/
│   │   └── gnome.nix                  # desktops.gnome.enable
│   └── shell.nix                      # shell.enable
└── hosts/
    └── pc/                            # Your machine
        ├── default.nix                # Host config (boot, network, packages)
        ├── hardware.nix               # Hardware config (generated)
        ├── vm-testing.nix             # VM settings (remove for real hardware)
        └── users/
            └── demo/                  # Your user
                ├── system.nix         # NixOS user definition (groups, shell)
                └── default.nix        # Home-manager config
```

## Why this approach?

### Library-based flake

All the flake machinery lives in [`nixos-lib`](../nixos-lib). Your `flake.nix` is just:

```nix
{
  description = "NixOS configuration";
  inputs.nixos-lib.url = "path:../nixos-lib";
  outputs = { self, nixos-lib, ... }:
    nixos-lib.lib.mkFlake { inherit self; };
}
```

When you publish the library, change the URL to `github:you/nixos-lib` — nothing else changes.

### Hosts are top-level

Hosts live at `hosts/<hostname>/`. Each host owns its users — adding a machine is just creating a directory:

```
hosts/
├── pc/           # existing
└── laptop/       # new — picked up automatically
    ├── default.nix
    ├── hardware.nix
    └── users/
        └── yourname/
            ├── system.nix
            └── default.nix
```

No changes to `flake.nix` needed.

### Users are self-contained

Each user directory under `hosts/<hostname>/users/<name>/` contains everything for that user:

- `system.nix` — NixOS user definition (`users.users.<name>`, groups, shell)
- `default.nix` — Home-manager config (packages, dotfiles, programs)

Adding a user to a host means creating one directory with two files. Nothing else to wire up.

### Auto-imported modules

All `.nix` files under `modules/` are automatically imported as NixOS modules via [`import-tree`](https://github.com/vic/import-tree). Enable what you need per host:

```nix
# hosts/pc/default.nix
desktops.gnome.enable = true;
shell.enable = true;
```

### Module pattern

Every module follows the same shape:

```nix
{ config, lib, pkgs, ... }:

let cfg = config.myFeature; in
{
  options.myFeature.enable = lib.mkEnableOption "description";

  config = lib.mkIf cfg.enable {
    # only applied when enabled
  };
}
```

All modules are always available but off by default. The LSP (`nixd`) shows the description from `mkEnableOption` as hover documentation.

---

## Getting Started

### 1. Rename placeholders

```bash
mv hosts/pc hosts/yourhostname
mv hosts/yourhostname/users/demo hosts/yourhostname/users/yourname
```

Then update `users.users.demo` → `users.users.yourname` in:
- `hosts/yourhostname/users/yourname/system.nix`
- `hosts/yourhostname/vm-testing.nix`

### 2. Generate hardware config

On your target machine (or NixOS installer):

```bash
nixos-generate-config --show-hardware-config > hosts/yourhostname/hardware.nix
```

### 3. Test in a VM first

```bash
just vm           # default host (pc)
just vm yourhostname
```

Login: user `demo` (or your renamed user), password `test`.

### 4. Install on real hardware

```bash
nix-shell -p git just
git clone <your-repo> /mnt/etc/nixos
cd /mnt/etc/nixos
sudo nixos-install --flake .#yourhostname
```

Remove the `vm-testing.nix` import from `hosts/yourhostname/default.nix` before deploying.

### 5. Apply changes

```bash
just switch
```

---

## Expanding the Configuration

### Adding a desktop environment

Uncomment in `hosts/yourhostname/default.nix`:

```nix
desktops.gnome.enable = true;
```

### Adding a NixOS module

Create a file in `modules/`:

```nix
# modules/my-feature.nix
{ config, lib, pkgs, ... }:

let cfg = config.myFeature; in
{
  options.myFeature.enable = lib.mkEnableOption "my feature description";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.htop ];
  };
}
```

Then `git add` it and enable in any host config:

```nix
myFeature.enable = true;
```

### Adding a home-manager module

Same as above but scoped to home-manager:

```nix
# modules/my-tool.nix
{ config, lib, pkgs, ... }:

let cfg = config.myTool; in
{
  options.myTool.enable = lib.mkEnableOption "my tool description";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.htop ];
  };
}
```

Enable in any user's `default.nix`:

```nix
myTool.enable = true;
```

### Adding another user to a host

```
hosts/pc/users/
├── demo/          # existing
└── alice/         # new — picked up automatically
    ├── system.nix
    └── default.nix
```

No changes to `flake.nix` or any host config needed.

### Adding another host

```
hosts/
├── pc/            # existing
└── laptop/        # new — picked up automatically
    ├── default.nix
    ├── hardware.nix
    ├── vm-testing.nix
    └── users/
        └── yourname/
            ├── system.nix
            └── default.nix
```

No changes to `flake.nix` needed.

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
| `just vm [host]` | Build and run host in a VM (default: pc) |
| `just check` | Check for errors without building |
| `just rollback` | Revert to previous generation |
| `just clean` | Remove old generations (30d+) |
| `just maintenance` | Clean + optimise store |

## VM Notes

The `vm-testing.nix` file configures the VM with 4GB RAM, 4 cores, 20GB disk, and KVM acceleration. Password is set to `test` for easy login.

Remove the `vm-testing.nix` import from `default.nix` before installing on real hardware.
