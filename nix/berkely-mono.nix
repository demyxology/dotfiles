# https://yildiz.dev/posts/packing-custom-fonts-for-nixos/

{ config, pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono-typeface";
  version = "1.009";

  src = ./berkeley-mono-typeface.zip;
  dst = if pkgs.system  == "aarch64-darwin" then "/Users/nikita/Library/fonts"
        else "/home/nikita/.local/share/fonts";

  unpackPhase = ''
    runHook preUnpack
    ${pkgs.unzip}/bin/unzip $src

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    install -Dm644 berkeley-mono-typeface/*.ttf -t $dst

    runHook postInstall
  '';
}
