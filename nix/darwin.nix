{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  programs.zsh = {
    enableFzfCompletion = true;
    enableFzfGit = true;
    enableFzfHistory = true;
  };

  environment.shellAliases = {
    update = "darwin-rebuild switch --flake ~/nix";
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
      "sonic-robo-blast-2"
      "utm"
      "visual-studio-code"
      "iterm2"
      "spotify"
      "steam"
      "warp"
    ];
    masApps = {
      "Messenger" = 454638411;
      "WhatsApp" = 310633997;
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
