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

  # Podman container registries configuration
  # This allows using short names like "hello-world" instead of "docker.io/library/hello-world"
  home.file.".config/containers/registries.conf".text = ''
    unqualified-search-registries = ["docker.io"]
    short-name-mode = "enforcing"
  '';
}
