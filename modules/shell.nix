{ pkgs, ... }:

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
    functions = {
      nix-just = {
        description = "Run just commands from nix-configuration directory with autocomplete";
        body = ''
          set -l nix_config_dir "$HOME/nix-configuration"

          if not test -d "$nix_config_dir"
            echo "Error: Nix configuration directory not found at $nix_config_dir"
            return 1
          end

          if not test -f "$nix_config_dir/justfile"
            echo "Error: justfile not found at $nix_config_dir/justfile"
            return 1
          end

          just --justfile "$nix_config_dir/justfile" --working-directory "$nix_config_dir" $argv
        '';
      };
    };
  };

  # Add Fish completion for nix-just that wraps just's completion
  home.file.".config/fish/completions/nix-just.fish".text = ''
    # Completions for nix-just - wraps just completions
    function __nix_just_complete
      set -l nix_config_dir "$HOME/nix-configuration"
      if test -f "$nix_config_dir/justfile"
        just --justfile "$nix_config_dir/justfile" --summary 2>/dev/null | string split ' '
      end
    end

    # Complete nix-just with recipe names
    complete -c nix-just -f -a "(__nix_just_complete)"
  '';

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
