{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    alacritty
    atuin
    bat
    btop
    cmake
    discord
    dust
    emacs
    fd
    findutils
    fish
    fzf
    gcc
    git
    helix
    irssi
    kakoune
    kitty
    lmstudio
    llvmPackages.libcxxClang
    llvmPackages.clang-tools
    mc
    neovide
    neovim
    nixfmt
    nmap
    nodejs
    nushell
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
    wezterm
    xonsh
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
    # fails due to deprecated dependency on fuse2
    # _9pfs
    ardour
    dolphin-emu
    drawterm
    file
    firefox
    fuzzel
    ghostty
    gnome-system-monitor
    gnome-weather
    lact
    pciutils
    protonup-qt
    obs-studio
    qbittorrent
    signal-desktop
    # FIXME: package broken -- contact moody?
    #tlsclient
    unzip
    vscode
    vulkan-tools
    xcape
    xclip
    xsel
  ];
}
