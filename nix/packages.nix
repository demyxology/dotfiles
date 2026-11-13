{ pkgs, ... }:

{

  commonPackages = with pkgs; [
    alacritty
    bat
    btop
    cmake
    dust
    drawterm
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
    omp
    pi-coding-agent
    plan9port
    pure-prompt
    pv
    python3
    qemu
    ripgrep
    rlwrap
    spotify
    tldr
    tlsclient
    tmux
    tree
    vim
    wezterm
    wget
    xonsh
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  darwinPackages = with pkgs; [
    chatgpt
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
    discord
    dolphin-emu
    drawterm
    emote
    file
    firefox
    fuzzel
    ghostty
    gnome-system-monitor
    gnome-weather
    lact
    libreoffice
    mangohud
    pulseaudio
    pciutils
    protonup-qt
    obs-studio
    qbittorrent
    signal-desktop
    steam
    unzip
    vscode
    vulkan-tools
    xcape
    xclip
    xsel
    zed-editor
  ];
}
