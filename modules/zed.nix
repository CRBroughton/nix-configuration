{ pkgs, ... }:

{
  # Zed editor with Vulkan dependencies
  home.packages = with pkgs; [
    zed-editor
    vulkan-tools
    vulkan-loader
    vulkan-validation-layers
    mesa
  ];

  home.file.".config/zed/settings.json".text = ''
    {
      "ui_font_size": 16,
      "ui_font_family": "FiraCode Nerd Font Mono",
      "buffer_font_weight": 600,
      "buffer_font_features": {
        "liga": true
      },
      "soft_wrap": "editor_width",
      "tab_size": 2,
      "relative_line_numbers": "enabled",
      "theme": {
        "mode": "dark",
        "light": "Vitesse Light Soft",
        "dark": "Vitesse Black"
      },
      "terminal": {
        "shell": {
          "program": "fish"
        }
      },
      "diagnostics": {
        "button": true,
        "include_warnings": true,
        "inline": {
          "enabled": true,
          "update_debounce_ms": 150,
          "padding": 4,
          "min_column": 0,
          "max_severity": null
        }
      },
      "formatter": {
        "code_actions": {
          "source.fixAll.eslint": true
        }
      }
    }
  '';
}
