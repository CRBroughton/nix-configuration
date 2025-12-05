{ ... }:

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

  # Set up environment variable for Docker compatibility with Podman
  home.sessionVariables = {
    DOCKER_HOST = "unix:///run/user/$UID/podman/podman.sock";
  };

  # Activation script to enable and start Podman socket
  home.activation.enablePodmanSocket = {
    after = [ "linkGeneration" ];
    before = [ "reloadSystemd" ];
    data = ''
      # Enable the system-provided podman.socket
      if ! /usr/bin/systemctl --user is-enabled podman.socket >/dev/null 2>&1; then
        $DRY_RUN_CMD /usr/bin/systemctl --user enable podman.socket
      fi

      # Start the socket if it's not active
      if ! /usr/bin/systemctl --user is-active podman.socket >/dev/null 2>&1; then
        $DRY_RUN_CMD /usr/bin/systemctl --user start podman.socket
      fi
    '';
  };
}
