{
  description = "workstation configs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      nixgl,
    }:
    let
      # Create a function to generate system-specific configurations
      mkSystem =
        systemType: configuration:
        if systemType == "darwin" then
          nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [
              ./common.nix
              configuration
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
