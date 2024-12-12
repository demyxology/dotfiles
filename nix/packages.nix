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
    nerdfonts
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
    _9pfs
    discord
    drawterm
    emacs
    file
    git
    google-chrome
    kitty
    kitty-themes
    monitor
    shutter
    spotify
    steam
    thunderbird
    vscode
  ];
}
