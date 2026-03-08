{ config, pkgs, ... }:

{
  home.file.".config/zed/settings.json".text = builtins.toJSON {
    ui_font_size = 16;
    ui_font_family = "FiraCode Nerd Font Mono";
    buffer_font_weight = 600;
    soft_wrap = "editor_width";
    tab_size = 2;
    relative_line_numbers = "enabled";
    theme = {
      mode = "dark";
      dark = "Vitesse Black";
    };
    terminal.shell.program = "fish";
  };

  home.packages = with pkgs; [
    zed-editor
    # Vulkan (required for Zed)
    vulkan-tools
    vulkan-loader
  ];
}
