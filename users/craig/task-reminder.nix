{ pkgs, ... }:
let
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

  tasks = [
    { every = 1;    task = "Check the sink for dishes"; }
    { time = "12:00"; task = "Lunch time!"; }
    { time = "21:30"; task = "Check the doors are locked and the PC is off"; }
  ];

  units = map mkReminder tasks;
in
{
  systemd.user.services = builtins.foldl' (acc: u: acc // u.services) {} units;
  systemd.user.timers   = builtins.foldl' (acc: u: acc // u.timers)   {} units;
}
