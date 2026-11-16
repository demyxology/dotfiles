# https://yildiz.dev/posts/packing-custom-fonts-for-nixos/

{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "1.009";

  src = ./berkeley-mono.zip;

  nativeBuildInputs = [ pkgs.nerd-font-patcher ];

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

  postInstall = ''
    mkdir -p "$out/share/fonts/truetype/berkeley-mono" "$out/share/fonts/truetype/berkeley-mono-nerd"
    mv "$out"/share/fonts/truetype/BerkeleyMono-*.ttf "$out/share/fonts/truetype/berkeley-mono/"
    for f in "$out"/share/fonts/truetype/berkeley-mono/*.ttf; do
      nerd-font-patcher --complete --outputdir "$out/share/fonts/truetype/berkeley-mono-nerd/" "$f"
    done
  '';
}
