{
  description = "workstation configs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    # fix mac gui apps
    # fix "failed to allocate bug" - https://amreis.github.io/misc/2026/09/15/macos27-nix-darwin.html
    # XXX: remove this when no longer necessary
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";  # temporarily override nixpkgs url to get SBCL v2.6.6
    };
    home-manager.url = "github:nix-community/home-manager";
    lazyvim.url = "github:pfassina/lazyvim-nix";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      nix-homebrew,
      mac-app-util,
      home-manager,
      lazyvim,
    }:
    let
      mkSystem =
        systemType: configuration:
        if systemType == "darwin" then
          nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [
              ./common.nix
              configuration
              nix-homebrew.darwinModules.nix-homebrew
              mac-app-util.darwinModules.default
              {
                nix-homebrew = {
                  enable = true;
                  enableRosetta = true;
                  user = "nikita";
                  autoMigrate = true;
                };
                nix.package = nixpkgs.legacyPackages.aarch64-darwin.nixVersions.latest;
              }
              (
                { pkgs, ... }:
                {
                  system.configurationRevision = self.rev or self.dirtyRev or null;
                  system.stateVersion = 5;
                  nixpkgs.hostPlatform = "aarch64-darwin";
                  nix.extraOptions = ''
                    extra-platforms = x86_64-darwin aarch64-darwin
                  '';
                }
              )
            ];
          }
        else
          nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              ./common.nix
              configuration
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = { inherit inputs; };
                home-manager.users.nikita = ./home.nix;
              }
            ];
          };
    in
    {
      darwinConfigurations."dad" = mkSystem "darwin" ./darwin.nix;

      nixosConfigurations.nixos = mkSystem "nixos" {
        imports = [
          ./hardware-configuration.nix
          ./configuration.nix
        ];
      };
    };
}
