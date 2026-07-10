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

  environment.shellAliases = {
    ll = "ls -l";
    e = "nvim";
  };

  nix = {
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
      NIX_AUTO_RUN_INTERACTIVE = 1;
    };
    shells = with pkgs; [ zsh ];
  };
}
