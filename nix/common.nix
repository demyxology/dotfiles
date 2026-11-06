{ pkgs, ... }:

{
  fonts.packages = [
    ((import ./berkeley-mono.nix { inherit pkgs; }).overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.nerd-font-patcher ];
      postInstall = ''
        mkdir -p "$out/share/fonts/truetype/berkeley-mono" "$out/share/fonts/truetype/berkeley-mono-nerd"
        mv "$out"/share/fonts/truetype/BerkeleyMono-*.ttf "$out/share/fonts/truetype/berkeley-mono/"
        for f in "$out"/share/fonts/truetype/berkeley-mono/*.ttf; do
          nerd-font-patcher --complete --outputdir "$out/share/fonts/truetype/berkeley-mono-nerd/" "$f"
        done
      '';
    }))
  ];

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
    nix-installed = "nix-store -q --references /var/run/current-system/sw | cut -d'-' -f2-";
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
      NIX_AUTO_RUN_INTERACTIVE = "1";
    };
    shells = with pkgs; [ zsh ];
  };
}
