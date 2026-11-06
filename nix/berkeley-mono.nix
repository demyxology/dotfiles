# https://yildiz.dev/posts/packing-custom-fonts-for-nixos/

{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "1.009";

  src = ./berkeley-mono.zip;

  unpackPhase = ''
    runHook preUnpack
    ${pkgs.unzip}/bin/unzip $src

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/share/fonts/truetype"
    install -m644 berkeley-mono/TTF/*.ttf "$out/share/fonts/truetype/"

    runHook postInstall
  '';
}
