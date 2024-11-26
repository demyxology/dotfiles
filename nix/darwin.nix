{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  environment.shellAliases = {
    ll = "ls -l";
    update = "sudo darwin-rebuild switch --flake ~/nix";
    e = "emacsclient";
    n = "nvim";
    enix = "e ~/nix/flake.nix";
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "uninstall";

    taps = [ ];
    brews = [ ];
    casks = [ "emacs" ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  nix.linux-builder.enable = true;

  documentation.man.enable = true;

  networking = {
    hostName = "dad";
    localHostName = "dad";
    computerName = "dad";
  };
}
