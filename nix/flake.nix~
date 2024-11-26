{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    nur = {
      url = "github:nix-community/NUR";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nur,
      nixos-hardware,
      ...
    }@inputs:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      lib = nixpkgs.lib;
      display = import ./display.nix { inherit pkgs; };
      fonts = import ./fonts.nix { inherit pkgs; };
      systemd = import ./systemd.nix { inherit pkgs; };
      zsh = import ./zsh.nix { inherit pkgs; };
      programs = import ./programs { inherit pkgs lib; };
      services = import ./services { inherit pkgs; };
    in
    {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        modules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-x220
          ./hardware-configuration.nix

          ./env.nix
          ./hw.nix
          ./mount.nix
          ./nixoscfg.nix
          ./syscfg.nix
          display
          fonts
          programs
          services
          systemd
          zsh
        ];
      };
    };
}
