{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.services.nixpkgsFirefoxSuggest;
  python = lib.getExe pkgs.python3;
  nix = lib.getExe pkgs.nix;
  script = "${./nixpkgs-suggest.py}";
  baseArgs = [
    "--port" (toString cfg.port)
    "--nix" nix
    "--source" cfg.source
  ];
  # Each wrapper embeds the script and exact arguments in the Nix store.
  server = pkgs.writeShellScript "nixpkgs-suggest-server" ''
    exec ${lib.escapeShellArgs ([ python script "serve" ] ++ baseArgs)}
  '';
  updater = pkgs.writeShellScript "nixpkgs-suggest-refresh" ''
    exec ${lib.escapeShellArgs ([ python script "refresh" ] ++ baseArgs)}
  '';
in
{
  options.services.nixpkgsFirefoxSuggest = {
    enable = mkEnableOption "localhost Firefox Nixpkgs suggestions";
    port = mkOption {
      type = types.port;
      default = 8765;
      description = "Localhost port used for Firefox's suggestion endpoint.";
    };
    source = mkOption {
      type = types.str;
      default = "github:NixOS/nixpkgs/nixos-unstable";
      description = "Upstream flake source to index independently of this configuration's lock file.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf pkgs.stdenv.hostPlatform.isLinux {
      systemd.user.services.nixpkgs-suggest-server = {
        Unit.Description = "Firefox Nixpkgs suggestion server";
        Service = {
          ExecStart = "${server}";
          Restart = "on-failure";
          RestartSec = 3;
        };
        Install.WantedBy = [ "default.target" ];
      };
      systemd.user.services.nixpkgs-suggest-refresh = {
        Unit.Description = "Refresh Nixpkgs suggestion index (background)";
        Service = {
          Type = "oneshot";
          ExecStart = "${updater}";
        };
      };
      systemd.user.timers.nixpkgs-suggest-refresh = {
        Unit.Description = "Check upstream nixos-unstable for suggestion index updates";
        Timer = {
          OnStartupSec = "2min";
          OnUnitActiveSec = "24h";
          Unit = "nixpkgs-suggest-refresh.service";
        };
        Install.WantedBy = [ "timers.target" ];
      };
    })
    (mkIf pkgs.stdenv.hostPlatform.isDarwin {
      launchd.agents.nixpkgs-suggest-server = {
        enable = true;
        config = {
          ProgramArguments = [ "${server}" ];
          RunAtLoad = true;
          KeepAlive = true;
        };
      };
      launchd.agents.nixpkgs-suggest-refresh = {
        enable = true;
        config = {
          ProgramArguments = [ "${updater}" ];
          RunAtLoad = true;
          StartInterval = 86400; # Every 24 hours, without blocking the rebuild.
        };
      };
    })
  ]);
}
