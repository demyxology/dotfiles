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
    discord
    drawterm
    emacs
    evolution
    file
    firefox
    ghostty
    gnome-remote-desktop
    obs-studio
    spotify
    steam
    tlsclient
    unzip
    ventoy-full
    wl-clipboard
    xclip
    xsel
  ];
}
