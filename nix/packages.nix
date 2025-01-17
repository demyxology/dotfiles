{ pkgs, ... }:

{
  commonPackages = with pkgs; [
    btop
    cmake
    fd
    findutils
    fzf
    git
    guile
    irssi
    llvmPackages_19.clang-tools
    llvmPackages_19.libcxxClang
    mu
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

  darwinPackages = with pkgs; [
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
