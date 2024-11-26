{ pkgs, ... }:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
in
{
  environment.systemPackages = commonPkgs.commonPackages ++ commonPkgs.darwinPackages;

  programs.zsh = {
    enable = true;
    enableFzfCompletion = true;
    enableFzfGit = true;
    enableFzfHistory = true;
    enableGlobalCompInit = true;
    enableCompletion = true;

    promptInit = ''
      # Initialize Pure prompt
      autoload -U promptinit && promptinit
      prompt pure
    '';
  };

  environment.shellAliases = {
    ll = "ls -l";
    update = "sudo darwin-rebuild switch --flake ~/nix";
    e = "emacsclient";
    n = "nvim";
    enix = "e ~/nix/flake.nix";
  };
  environment.shells = with pkgs; [ zsh ];

  services.emacs = {
    enable = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "uninstall";

    taps = [ ];
    brews = [ ];
    casks = [ "emacs" ];
  };

  environment.variables = {
    EDITOR = "emacsclient";
  };

  nix.gc.automatic = true;
  nix.optimise.automatic = true;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
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
