{ pkgs, ... }:

{
  fonts.packages = [
    (import ./berkeley-mono.nix { inherit pkgs; })
  ];

  programs.zsh = {
    enable = true;
    enableGlobalCompInit = false;
    promptInit = "";
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
    };
    shells = with pkgs; [ zsh ];
  };
}
