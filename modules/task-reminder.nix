{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.taskReminder;

  mkReminder =
    { time ? null, every ? null, task }:
    let
      safeTask = builtins.replaceStrings [ " " ":" "/" ] [ "-" "-" "-" ] task;
      safeKey =
        if every != null then "every-${toString every}h" else builtins.replaceStrings [ ":" ] [ "-" ] time;
      unitName = "task-reminder-${safeKey}-${safeTask}";
      onCalendar =
        if every != null then "*-*-* 00/${toString every}:00:00" else "*-*-* ${time}:00";
    in
    {
      services.${unitName} = {
        Unit.Description = "Task reminder: ${task}";
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.libnotify}/bin/notify-send 'Task Reminder' '${task}' --urgency=normal --app-name='Task Reminder'";
        };
        Install.WantedBy = [ "default.target" ];
      };
      timers.${unitName} = {
        Unit.Description = "Timer for task reminder: ${task}";
        Timer = {
          OnCalendar = onCalendar;
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
    };

  units = map mkReminder cfg.tasks;
in
{
  options.modules.taskReminder = {
    enable = lib.mkEnableOption "Task reminder notifications";

    tasks = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          time = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Time in HH:MM (24h). Mutually exclusive with 'every'.";
            example = "09:00";
          };
          every = lib.mkOption {
            type = lib.types.nullOr lib.types.ints.positive;
            default = null;
            description = "Repeat every N hours. Mutually exclusive with 'time'.";
            example = 2;
          };
          task = lib.mkOption {
            type = lib.types.str;
            description = "Notification body text.";
            example = "Take your medication";
          };
        };
      });
      default = [ ];
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = map (t: {
      assertion = (t.time != null) != (t.every != null);
      message = "task-reminder: each task needs exactly one of 'time' or 'every' (task: \"${t.task}\")";
    }) cfg.tasks;

    home-manager.users.${user} = {
      systemd.user.services = builtins.foldl' (acc: u: acc // u.services) { } units;
      systemd.user.timers = builtins.foldl' (acc: u: acc // u.timers) { } units;
    };
  };
}
