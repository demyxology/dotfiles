{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  system.primaryUser = "nikita";
  users.users.nikita.home = "/Users/nikita";

  environment.shellAliases = {
    update = "nix flake update --flake ~/nix/ && sudo darwin-rebuild switch --flake ~/nix";
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";

    taps = [ ];
    brews = [ ];
    #XXX: some nix packages misbehave on macos. check in on them to see if theyre fixed
    casks = [
      "dolphin" # fails to build
      "firefox" # crashes on signin, firefox-bin doesnt launch
      "steam" # no nix-darwin candidate
    ];
  };

  system.defaults = {
    dock = {
      autohide = true;
      mineffect = "scale";
      minimize-to-application = true;
      static-only = true;
    };
  };

  security.pam.services.sudo_local.touchIdAuth = true;

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
