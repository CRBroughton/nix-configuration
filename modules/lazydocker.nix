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

  # Create systemd socket and service for Podman API
  # This works on all systems, whether Podman is from Nix or system packages
  systemd.user.sockets.podman = {
    Unit = {
      Description = "Podman API Socket";
      Documentation = "man:podman-system-service(1)";
    };
    Socket = {
      ListenStream = "%t/podman/podman.sock";
      SocketMode = "0660";
    };
    Install = {
      WantedBy = [ "sockets.target" ];
    };
  };

  systemd.user.services.podman = {
    Unit = {
      Description = "Podman API Service";
      Documentation = "man:podman-system-service(1)";
      Requires = "podman.socket";
      After = "podman.socket";
    };
    Service = {
      Type = "exec";
      KillMode = "process";
      Environment = "LOGGING=--log-level=info";
      ExecStart = "${pkgs.podman}/bin/podman $LOGGING system service";
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
