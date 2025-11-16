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
  '';
}
