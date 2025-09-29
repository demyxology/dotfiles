{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  system.primaryUser = "nikita";

  programs.zsh = {
    enableFzfCompletion = true;
    enableFzfGit = true;
    enableFzfHistory = true;
  };

  environment.shellAliases = {
    update = "nix flake update --flake ~/nix/; sudo darwin-rebuild switch --flake ~/nix";
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";

    taps = [ ];
    brews = [ ];
    casks = [
      "discord"
      "emacs"
      "firefox"
      "ghostty"
      "neovide"
      "sonic-robo-blast-2"
      "transmission"
      "utm"
      "visual-studio-code"
      "iterm2"
      "signal"
      "spotify"
      "steam"
      "warp"
    ];
    masApps = {
      "Messenger" = 454638411;
      "WhatsApp" = 310633997;
    };
  };

  launchd.user.agents.emacs = {
    serviceConfig = {
      Label = "gnu.emacs.daemon";
      ProgramArguments = [
        "/Applications/Emacs.app/Contents/MacOS/Emacs"
        "--daemon"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/tmp/emacs-daemon.log";
      StandardErrorPath = "/tmp/emacs-daemon.error.log";
    };
  };
  
  system.defaults = {
    dock = {
      autohide = true;
      mineffect = "scale";
      minimize-to-application = true;
      static-only = true;
    };
  };

  nixpkgs.config.allowUnsupportedSystem = true;

  security.pam.services.sudo_local.touchIdAuth = true;

  nix.linux-builder.enable = true;

  documentation.man.enable = true;

  networking = {
    hostName = "dad";
    localHostName = "dad";
    computerName = "dad";
  };
}
