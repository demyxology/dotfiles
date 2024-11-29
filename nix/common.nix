# common/common.nix
{ pkgs, ... }:

{
  # Shared ZSH configuration
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = true;
    enableCompletion = true;

    promptInit = ''
      # Initialize Pure prompt
      autoload -U promptinit && promptinit
      prompt pure
    '';
  };

  # Common shell aliases
  environment.shellAliases = {
    ll = "ls -l";
    e = "emacsclient";
    n = "nvim";
  };

  # Emacs service
  services.emacs = {
    enable = true;
  };

  # Common Nix settings
  nix = {
    gc.automatic = true;
    optimise.automatic = true;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  # Common package settings
  nixpkgs.config = {
    allowUnfree = true;
  };

  # Common environment settings
  environment = {
    variables = {
      EDITOR = "emacsclient";
    };
    shells = with pkgs; [ zsh ];
  };
}
