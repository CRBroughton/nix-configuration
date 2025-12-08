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
      just = {
        description = "Run just - uses local justfile if present, otherwise nix-configuration justfile";
        wraps = "just";
        body = ''
          # Check for local justfile first
          if test -f justfile; or test -f Justfile
            command just $argv
            return
          end

          # Fall back to nix-configuration justfile
          set -l nix_config_dir "$HOME/nix-configuration"

          if not test -d "$nix_config_dir"
            echo "Error: Nix configuration directory not found at $nix_config_dir"
            return 1
          end

          if not test -f "$nix_config_dir/justfile"
            echo "Error: justfile not found at $nix_config_dir/justfile"
            return 1
          end

          command just --justfile "$nix_config_dir/justfile" --working-directory "$nix_config_dir" $argv
        '';
      };
    };
  };

  # Add Fish completion for just - uses local or nix-configuration justfile
  home.file.".config/fish/completions/just.fish".text = ''
    # Completions for just - local justfile takes precedence
    function __just_complete
      # Check for local justfile first
      if test -f justfile; or test -f Justfile
        command just --summary 2>/dev/null | string split ' '
        return
      end

      # Fall back to nix-configuration justfile
      set -l nix_config_dir "$HOME/nix-configuration"
      if test -f "$nix_config_dir/justfile"
        command just --justfile "$nix_config_dir/justfile" --summary 2>/dev/null | string split ' '
      end
    end

    # Complete just with recipe names
    complete -c just -f -a "(__just_complete)"
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
