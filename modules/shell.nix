{ ... }:

{
  # Fish shell configuration
  programs.fish = {
    enable = true;
    # shellInit = ''
    #   # Add development tools to PATH
    #   fish_add_path ~/.bun/bin
    #   fish_add_path ~/.volta/bin
    #   fish_add_path ~/.local/go/bin
    #   fish_add_path ~/.local/zig
    # '';
  };

  # Starship prompt
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
  };

  # Git configuration (basic)
  programs.git = {
    enable = true;
  };
}
