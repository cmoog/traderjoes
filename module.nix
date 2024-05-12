{ pkgs, config, lib, ... }:

let
  cfg = config.traderjoes;
in
{

  options.traderjoes = {
    enable = lib.mkEnableOption "trader joes price fetching service";
    workingDir = lib.mkOption {
      type = lib.types.str;
      description = "Directory to write price DB and site files.";
    };
    user = lib.mkOption {
      type = lib.types.str;
    };
  };
  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [
      (_: prev: {
        traderjoes = prev.callPackage ./. { };
      })
    ];
    systemd.timers.traderjoes = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = false;
        Unit = "traderjoes.service";
      };
    };
    systemd.services.traderjoes = {
      path = with pkgs; [
        sqlite
        nodePackages.wrangler
        glibc # needed by wrangler
        traderjoes
      ];
      serviceConfig = {
        ExecStart = ./cron.bash;
        Type = "oneshot";
        User = cfg.user;
        WorkingDirectory = cfg.workingDir;
      };
    };
  };
}
