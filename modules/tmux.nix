{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.tmux;

  # Vitesse Dark palette
  bg        = "#121212";
  bg_surface = "#181818";
  border    = "#191919";
  fg        = "#dbd7ca";
  fg_muted  = "#bfbaaa";
  fg_dim    = "#959da5";
  green     = "#4d9375";
  red       = "#cb7676";
  orange    = "#d4976c";
  blue      = "#6394bf";
in
{
  options.modules.tmux = {
    enable = lib.mkEnableOption "tmux multiplexer";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      programs.tmux = {
        enable = true;
        shell = "${pkgs.fish}/bin/fish";
        terminal = "tmux-256color";
        historyLimit = 10000;
        escapeTime = 0;
        keyMode = "vi";
        prefix = "C-a";
        mouse = true;
        baseIndex = 1;

        extraConfig = ''
          # True colour support
          set -ag terminal-overrides ",xterm-256color:RGB"
          set -ag terminal-overrides ",ghostty:RGB"

          # Pane numbering starts at 1
          setw -g pane-base-index 1
          set -g renumber-windows on

          # Split panes with v and s
          bind v split-window -h -c "#{pane_current_path}"
          bind s split-window -v -c "#{pane_current_path}"
          unbind '"'
          unbind %

          # New window keeps current path
          bind c new-window -c "#{pane_current_path}"

          # Arrow key pane navigation
          bind Up    select-pane -U
          bind Down  select-pane -D
          bind Left  select-pane -L
          bind Right select-pane -R

          # Resize panes with arrow keys (repeatable)
          bind -r M-Up    resize-pane -U 5
          bind -r M-Down  resize-pane -D 5
          bind -r M-Left  resize-pane -L 5
          bind -r M-Right resize-pane -R 5

          # Reload config
          bind r source-file ~/.config/tmux/tmux.conf \; display "Config reloaded"

          # vi-style copy mode
          bind -T copy-mode-vi v send -X begin-selection
          bind -T copy-mode-vi y send -X copy-selection-and-cancel

          # ── Vitesse Dark theme ──────────────────────────────────────────────

          # Pane borders
          set -g pane-border-style          "fg=${border}"
          set -g pane-active-border-style   "fg=${green}"

          # Message / command bar
          set -g message-style              "fg=${fg},bg=${bg_surface}"
          set -g message-command-style      "fg=${fg},bg=${bg_surface}"

          # Mode (copy mode) highlight
          set -g mode-style                 "fg=${bg},bg=${green}"

          # Status bar
          set -g status                     on
          set -g status-position            bottom
          set -g status-interval            5
          set -g status-style               "fg=${fg_muted},bg=${bg}"
          set -g status-left-length         40
          set -g status-right-length        80

          set -g status-left  "#[fg=${bg},bg=${green},bold] #S #[fg=${green},bg=${bg},nobold]"
          set -g status-right "#[fg=${border},bg=${bg}]#[fg=${fg_dim},bg=${border}] %H:%M #[fg=${bg_surface},bg=${border}]#[fg=${fg_muted},bg=${bg_surface}] %d %b #[fg=${bg},bg=${bg_surface}]"

          # Window tabs
          set -g window-status-format          "#[fg=${bg},bg=${bg}] #[fg=${fg_dim},bg=${bg}]#I #[fg=${fg_dim}]#W "
          set -g window-status-current-format  "#[fg=${bg},bg=${bg_surface}] #[fg=${fg},bg=${bg_surface},bold]#I #[fg=${green},bg=${bg_surface}]#W #[fg=${bg_surface},bg=${bg}]"
          set -g window-status-separator       ""

          # Window status flags (activity, bell)
          set -g window-status-activity-style  "fg=${orange},bg=${bg}"
          set -g window-status-bell-style       "fg=${red},bg=${bg},bold"
        '';
      };
    };
  };
}
