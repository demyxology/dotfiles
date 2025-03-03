{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    atuin
    bat
    btop
    cmake
    dust
    fd
    findutils
    fzf
    gcc
    git
    irssi
    isync
    llvmPackages_19.clang-tools
    llvmPackages_19.libcxxClang
    nixfmt-rfc-style
    nmap
    nodejs
    mc
    plan9port
    pure-prompt
    pv
    qemu
    ripgrep
    rlwrap
    tldr
    tmux
    tree
    zellij
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  darwinPackages = with pkgs; [
    coreutils-prefixed
  ];

  nixosPackages = with pkgs; [
    _9pfs
    alsa-utils
    discord
    drawterm
    emacs
    file
    firefox
    ghostty
    kitty
    kitty-themes
    monitor
    nitrogen
    shutter
    spotify
    steam
    thunderbird
    unzip
    vscode
  ];
}
