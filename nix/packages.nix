{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    atuin
    bat
    btop
    cmake
    discord
    dust
    emacs
    fd
    findutils
    fzf
    gcc
    git
    helix
    irssi
    kakoune
    llvmPackages.libcxxClang
    llvmPackages.clang-tools
    mc
    neovide
    neovim
    nixfmt-rfc-style
    nmap
    nodejs
    plan9port
    pure-prompt
    pv
    qemu
    ripgrep
    rlwrap
    spotify
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
    ghostty-bin
    iterm2
    utm
    vlc-bin
  ];

  nixosPackages = with pkgs; [
    _9pfs
    drawterm
    evolution
    file
    firefox
    fuzzel
    ghostty
    protonup-qt
    obs-studio
    orca
    signal-desktop
    tlsclient
    unzip
    vscode
    xclip
    xsel
  ];
}
