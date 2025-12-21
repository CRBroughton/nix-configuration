{
  description = "SearXNG metasearch engine with Docker container support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # SearXNG Python package
        searxng = pkgs.python3.pkgs.buildPythonApplication rec {
          pname = "searxng";
          version = "2024.12.15";

          src = pkgs.fetchFromGitHub {
            owner = "searxng";
            repo = "searxng";
            rev = version;
            hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # Will need to update
          };

          propagatedBuildInputs = with pkgs.python3.pkgs; [
            babel
            certifi
            python-dateutil
            fasttext-predict
            flask
            flask-babel
            brotli
            jinja2
            lxml
            pygments
            pyyaml
            redis
            httpx
            httpx-socks
            setproctitle
            uvloop
            httptools
            h11
          ];

          # Skip tests for now
          doCheck = false;

          meta = {
            description = "Privacy-respecting metasearch engine";
            homepage = "https://github.com/searxng/searxng";
          };
        };

        # Default configuration
        defaultSettings = pkgs.writeText "settings.yml" ''
          use_default_settings: true
          server:
            secret_key: "changeme"
            limiter: false
            image_proxy: true
          ui:
            static_use_hash: true
          search:
            safe_search: 0
            autocomplete: ""
            default_lang: ""
        '';

        # Startup script
        startupScript = pkgs.writeShellScript "start-searxng" ''
          export SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml

          # Create cache directory if it doesn't exist
          mkdir -p /var/cache/searxng

          # Start SearXNG
          exec ${searxng}/bin/searxng-run
        '';

        # Docker image
        dockerImage = pkgs.dockerTools.buildImage {
          name = "searxng";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "searxng-root";
            paths = [
              searxng
              pkgs.coreutils
              pkgs.bash
            ];
            pathsToLink = [ "/bin" ];
          };

          config = {
            Cmd = [ "${startupScript}" ];
            ExposedPorts = {
              "8080/tcp" = { };
            };
            Env = [
              "SEARXNG_SETTINGS_PATH=/etc/searxng/settings.yml"
            ];
            WorkingDir = "/";
          };
        };

        # Podman/Docker management scripts
        startContainer = pkgs.writeShellScriptBin "searxng-start" ''
          CONFIG_DIR="$HOME/.config/searxng"
          DATA_DIR="$HOME/.local/share/searxng"

          # Create directories if they don't exist
          mkdir -p "$CONFIG_DIR" "$DATA_DIR"

          # Create default settings if not exists
          if [ ! -f "$CONFIG_DIR/settings.yml" ]; then
            cat > "$CONFIG_DIR/settings.yml" <<'EOF'
          ${builtins.readFile defaultSettings}
          EOF
            echo "Created default configuration at $CONFIG_DIR/settings.yml"
            echo "Please update the secret_key before using in production!"
          fi

          # Load the image if not already loaded
          if ! ${pkgs.podman}/bin/podman image exists searxng:latest 2>/dev/null; then
            echo "Loading SearXNG image..."
            ${pkgs.podman}/bin/podman load < ${dockerImage}
          fi

          # Stop and remove existing container if running
          ${pkgs.podman}/bin/podman stop searxng 2>/dev/null || true
          ${pkgs.podman}/bin/podman rm searxng 2>/dev/null || true

          # Start the container
          echo "Starting SearXNG container..."
          ${pkgs.podman}/bin/podman run --name searxng -d \
            -p 8888:8080 \
            -v "$CONFIG_DIR:/etc/searxng:ro" \
            -v "$DATA_DIR:/var/cache/searxng" \
            searxng:latest

          echo "✓ SearXNG is now running at http://localhost:8888"
          echo ""
          echo "To view logs: podman logs -f searxng"
          echo "To stop: searxng-stop"
        '';

        stopContainer = pkgs.writeShellScriptBin "searxng-stop" ''
          echo "Stopping SearXNG container..."
          ${pkgs.podman}/bin/podman stop searxng
          ${pkgs.podman}/bin/podman rm searxng
          echo "✓ SearXNG container stopped"
        '';

        statusContainer = pkgs.writeShellScriptBin "searxng-status" ''
          ${pkgs.podman}/bin/podman ps -a --filter name=searxng
        '';

      in
      {
        packages = {
          default = dockerImage;
          docker-image = dockerImage;
          searxng = searxng;
          start = startContainer;
          stop = stopContainer;
          status = statusContainer;
        };
      }
    )
    // {
      # Export Home Manager module at the top level
      homeManagerModules.default =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          system = pkgs.stdenv.hostPlatform.system;
        in
        {
          options.programs.searxng = {
            enable = lib.mkEnableOption "SearXNG metasearch engine";

            port = lib.mkOption {
              type = lib.types.int;
              default = 8888;
              description = "Port to expose SearXNG on";
            };

            settings = lib.mkOption {
              type = lib.types.attrs;
              default = { };
              description = "SearXNG configuration settings";
            };
          };

          config = lib.mkIf config.programs.searxng.enable {
            home.packages = [
              self.packages.${system}.start
              self.packages.${system}.stop
              self.packages.${system}.status
            ];

            # Create config directory
            home.file.".config/searxng/.keep".text = "";

            # Create settings file if custom settings provided
            home.file.".config/searxng/settings.yml" = lib.mkIf (config.programs.searxng.settings != { }) {
              text = builtins.toJSON config.programs.searxng.settings;
            };
          };
        };
    };
}
