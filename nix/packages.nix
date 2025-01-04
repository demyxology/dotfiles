{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    bat
    btop
    fzf
    gnutls
    guile
    helix
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
    tmux
    zellij
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
    alsa-utils
    bluez
    discord
    drawterm
    emacs
    file
    firefox
    git
    kitty
    kitty-themes
    libgcc
    monitor
    nitrogen
    python313Packages.ds4drv
    shutter
    spotify
    steam
    thunderbird
    unzip
    vscode
    whatsapp-for-linux
  ];
}
