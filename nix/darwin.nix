{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  system.primaryUser = "nikita";

  #XXX want to move this to common but nixos doesnt share these?
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
    #XXX: some nix packages misbehave on macos. check in on them to see if theyre fixed
    casks = [ 
      "appcleaner" # installs but literally doesnt appear??
      "firefox" # crashes on signin, firefox-bin doesnt launch
      "signal" # base app fails to build, bin version is woefully out of date
      "steam" # no nix-darwin candidate
      "transmission" # gui not on nixpkgs
      "visual-studio-code" # fails to build
    ];
    # slow asf when updating, even if there isnt a new version
    /*
    masApps = {
      "Messenger" = 454638411;
      "WhatsApp" = 310633997;
    };
    */
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

  # remember to set the login shell with
  # chsh -s /run/current-system/sw/bin/zsh
  # as nix-darwin can't (?) do it for you!
}
