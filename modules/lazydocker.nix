{ pkgs, ... }:

{
  # Lazydocker configuration
  home.file.".config/lazydocker/config.yml".text = ''
    gui:
      theme:
        activeBorderColor:
          - yellow
          - bold
        optionsTextColor:
          - yellow
          - bold
      sidePanelWidth: 0.33
    commandTemplates:
      dockerCompose: podman-compose
  '';

  # Podman container registries configuration
  # This allows using short names like "hello-world" instead of "docker.io/library/hello-world"
  home.file.".config/containers/registries.conf".text = ''
    unqualified-search-registries = ["docker.io"]
    short-name-mode = "enforcing"
  '';

  # Container image trust policy
  # Matches Fedora's default policy - accepts images without signature verification
  home.file.".config/containers/policy.json".text = ''
    {
        "default": [
            {
                "type": "insecureAcceptAnything"
            }
        ],
        "transports":
            {
                "docker-daemon":
                    {
                        "": [{"type":"insecureAcceptAnything"}]
                    }
            }
    }
  '';

  # Set up environment variable for Docker compatibility with Podman
  home.sessionVariables = {
    DOCKER_HOST = "unix:///run/user/$UID/podman/podman.sock";
  };

  # Activation script to enable and start Podman socket
  home.activation.enablePodmanSocket = {
    after = [ "linkGeneration" ];
    before = [ "reloadSystemd" ];
    data = ''
      # Try to find systemctl - check both system and Nix paths
      SYSTEMCTL=""
      if command -v /usr/bin/systemctl &> /dev/null; then
        SYSTEMCTL="/usr/bin/systemctl"
      elif command -v systemctl &> /dev/null; then
        SYSTEMCTL="systemctl"
      else
        echo "Warning: systemctl not found, skipping podman.socket setup"
        exit 0
      fi

      # Enable the podman.socket if available
      if $SYSTEMCTL --user list-unit-files podman.socket &> /dev/null; then
        if ! $SYSTEMCTL --user is-enabled podman.socket >/dev/null 2>&1; then
          $DRY_RUN_CMD $SYSTEMCTL --user enable podman.socket
        fi

        # Start the socket if it's not active
        if ! $SYSTEMCTL --user is-active podman.socket >/dev/null 2>&1; then
          $DRY_RUN_CMD $SYSTEMCTL --user start podman.socket
        fi
      else
        echo "Warning: podman.socket unit not found, skipping"
      fi
    '';
  };
}
