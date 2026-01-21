{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    atuin
    bat
    btop
    cmake
    discord
    dolphin-emu
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
    nixfmt
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
    ardour
    drawterm
    evolution
    file
    firefox
    fuzzel
    ghostty
    protonup-qt
    rpcs3
    obs-studio
    qbittorrent
    signal-desktop
    # FIXME: package broken -- contact moody?
    #tlsclient
    unzip
    vscode
    xclip
    xsel
  ];
}
