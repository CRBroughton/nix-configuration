{ ... }:

{
  # Fish shell configuration
  programs.fish = {
    enable = true;
    shellAliases = {
      cd = "z";
      cat = "bat";
      ls = "eza";
    };
    shellInit = ''
      set -gx CGO_ENABLED 1
    '';
  };

  # Starship prompt
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
  };

  # Zoxide (smarter cd)
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };
}
