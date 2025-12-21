# SearXNG Nix Flake

A Nix flake for running SearXNG, a privacy-respecting metasearch engine, in a container using Podman.

## Features

- Declarative Docker image built with Nix
- Automatic container management scripts
- Persistent configuration and cache directories
- Privacy-focused default settings

## Quick Start

1. Enable the module in your Home Manager configuration:

```nix
programs.searxng.enable = true;
```

2. After `home-manager switch`, start SearXNG:

```bash
searxng-start
```

3. Access SearXNG at [http://localhost:8888](http://localhost:8888)

## Management Commands

- `searxng-start` - Start the SearXNG container
- `searxng-stop` - Stop and remove the container
- `searxng-status` - Check container status

## Configuration

Configuration file location: `~/.config/searxng/settings.yml`

A default configuration is created automatically on first run. See `settings.yml.example` for customization options.

**Important**: Change the `secret_key` in your settings file before using in production!

## Data Directories

- Configuration: `~/.config/searxng/`
- Cache/Data: `~/.local/share/searxng/`

## Customizing Settings

Edit `~/.config/searxng/settings.yml` to customize:
- Search engines
- UI theme
- Safe search level
- Language preferences
- And more

After changing settings, restart the container:

```bash
searxng-stop
searxng-start
```

## Advanced Usage

### Building the Docker Image

```bash
nix build .#docker-image
```

### Loading the Image Manually

```bash
podman load < result
```

### Custom Port

The container exposes port 8888 by default. To change this, modify the `-p` flag in the start script or customize in your Home Manager config:

```nix
programs.searxng = {
  enable = true;
  port = 9999;
};
```

## Documentation

- [SearXNG Documentation](https://docs.searxng.org/)
- [SearXNG GitHub](https://github.com/searxng/searxng)
