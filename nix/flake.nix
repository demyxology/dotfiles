
{
  description = "macos config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
    }:
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."dad" = nix-darwin.lib.darwinSystem {
        modules = [ ./darwin.nix ];
      };

      darwinPackages = self.darwinConfigurations."dad".pkgs;
    };
}
