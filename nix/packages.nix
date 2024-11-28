{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    bat
    fzf
    gnutls
    irssi
    llvmPackages_19.clang-tools
    llvmPackages_19.libcxxClang
    neovim
    nixfmt-rfc-style
    nmap
    plan9port
    pure-prompt
    qemu
    ripgrep
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  darwinPackages =
    with pkgs;
    [
    ];

  nixosPackages = with pkgs; [
    alacritty
    discord
    drawterm
    emacs
    evolution
    file
    git
    google-chrome
    kitty
    kitty-themes
    mesa
    spotify
    steam
    vscode
  ];
}
