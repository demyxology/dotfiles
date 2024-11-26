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
      darwin-conf =
        { pkgs, ... }:
        {
          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = with pkgs; [
            fzf
            gnutls
            llvmPackages_19.clang-tools
            llvmPackages_19.libcxxClang
            neovim
            nixfmt-rfc-style
            nmap
            plan9port
            pure-prompt
            zsh
            zsh-autosuggestions
            zsh-syntax-highlighting
          ];

          programs.zsh = {
            enable = true;
            enableFzfCompletion = true;
            enableFzfGit = true;
            enableFzfHistory = true;
            enableGlobalCompInit = true;
            enableCompletion = true;

            promptInit = ''
              # Initialize Pure prompt
              autoload -U promptinit && promptinit
              prompt pure
            '';
          };

          environment.shellAliases = {
            ll = "ls -l";
            update = "sudo darwin-rebuild switch --flake ~/nix";
            e = "emacsclient";
            n = "nvim";
            enix = "e ~/nix/flake.nix";
          };
          environment.shells = with pkgs; [ zsh ];

          services.emacs = {
            enable = true;
          };

          nixpkgs.config = {
            allowUnfree = true;
          };

          homebrew = {
            enable = true;
            onActivation.cleanup = "uninstall";

            taps = [ ];
            brews = [ ];
            casks = [ "emacs" ];
          };

          environment.variables = {
            EDITOR = "emacsclient";
          };

          nix.gc.automatic = true;
          nix.optimise.automatic = true;

          # Necessary for using flakes on this system.
          nix.settings.experimental-features = [
            "nix-command"
            "flakes"
          ];
          # Auto upgrade nix package and the daemon service.
          services.nix-daemon.enable = true;

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 5;

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";

          nix.extraOptions = ''
            extra-platforms = x86_64-darwin aarch64-darwin
          '';

          security.pam.enableSudoTouchIdAuth = true;

          nix.linux-builder.enable = true;

          documentation.man.enable = true;

          networking = {
            hostName = "dad";
            localHostName = "dad";
            computerName = "dad";
          };
        };
    in
    {
      darwinConfigurations."dad" = nix-darwin.lib.darwinSystem {
        modules = [ darwin-conf ];
      };

      darwinPackages = self.darwinConfigurations."dad".pkgs;
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hardware-configuration.nix
          ./configuration.nix
        ];
      };
    };
}
