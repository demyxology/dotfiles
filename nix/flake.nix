{
  description = "workstation configs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

    outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
    }:
    let
      # Create a function to generate system-specific configurations
      mkSystem = systemType: configuration: 
        if systemType == "darwin" then 
          nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [ configuration ];
          }
        else 
          nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [ configuration ];
          };
    in 
    {
      darwinConfigurations."dad" = mkSystem "darwin" ./darwin/configuration.nix;
      
      nixosConfigurations.nixos = mkSystem "nixos" {
        imports = [
          ./hardware-configuration.nix
          ./configuration.nix
        ];
      };
    };
}
