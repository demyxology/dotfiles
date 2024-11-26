{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    fzf
    gnutls
    llvmPackages_19.clang-tools
    llvmPackages_19.libcxxClang
    neovim
    nixfmt-rfc-style
    nmap
    plan9port
    pure-prompt
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  darwinPackages =
    with pkgs;
    [
    ];

  nixosPackages =
    with pkgs;
    [
          discord
    drawterm
    emacs
    evolution
    git
    google-chrome
    vscode
    whatsapp-for-linux
    ];
}
