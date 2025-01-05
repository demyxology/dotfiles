{
  description = "workstation configs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      nix-homebrew,
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
              nix-homebrew.darwinModules.nix-homebrew
              {
                nix-homebrew = {
                  enable = true;
                  enableRosetta = true;
                  user = "nikita";
                  autoMigrate = true;
                };
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
