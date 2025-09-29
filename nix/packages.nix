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
    neovim
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
    fuzzel
    ghostty
    gnome-remote-desktop
    helix
    neovide
    kakoune
    lmstudio
    obs-studio
    orca
    signal-desktop
    spotify
    steam
    tlsclient
    unzip
    wl-clipboard
    xclip
    xsel
  ];
}
