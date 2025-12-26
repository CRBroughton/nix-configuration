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
}
