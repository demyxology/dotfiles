{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    atuin
    bat
    btop
    dust
    fd
    findutils
    fzf
    gcc
    git
    irssi
    llvmPackages_19.clang-tools
    llvmPackages_19.libcxxClang
    nixfmt-rfc-style
    nmap
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
    vim
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
    monitor
    nitrogen
    shutter
    spotify
    steam
    thunderbird
    unzip
  ];
}
