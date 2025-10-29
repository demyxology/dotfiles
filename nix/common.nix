{ pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = true;
    enableCompletion = true;

    promptInit = ''
      autoload -U promptinit && promptinit
      prompt pure
    '';
  };

  services.emacs = {
    enable = true;
  };

  environment.shellAliases = {
    ll = "ls -l";
    e = "nvim";
  };

  nix = {
    gc.automatic = true;
    optimise.automatic = true;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment = {
    variables = {
      EDITOR = "nvim";
    };
    shells = with pkgs; [ zsh ];
  };
}
