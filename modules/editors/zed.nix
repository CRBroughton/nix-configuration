# Zed editor
{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.editors.zed;
in
{
  options.modules.editors.zed = {
    enable = lib.mkEnableOption "Zed editor";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
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
    };
  };
}
