# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  ...
}:

let
  commonPkgs = import ./packages.nix { inherit pkgs; };
  berkeley-mono = import ./berkeley-mono.nix { inherit pkgs; };
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Bootloader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "ntsync" ];
  };

  fileSystems."/".options = [ "noatime" ];

  services.udev.extraRules = ''
    KERNEL=="ntsync", MODE="0660", GROUP="ntsync"
  '';
  #SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="0666"
  #  '';

  # powerManagement.cpuFreqGovernor = "performance";

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = true;
    desktopManager.cinnamon = {
      enable = true;
    };
    xkb = {
      layout = "us";
      variant = "";
    };
  };

  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/cinnamon/desktop/input-sources" = {
          xkb-options = [ "ctrl:nocaps" ];
        };
      };
    }
  ];

  systemd.user.services.xcape = {
    description = "xcape - Caps Lock as Escape when tapped";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.xcape}/bin/xcape -e 'Control_L=Escape' -t 100";
      Restart = "always";
      RestartSec = 3;
    };
  };

  services.cinnamon.apps.enable = true;

  # programs.hyprland = {
  #   enable = true;
  #   withUWSM = true; # recommended for most users
  #   xwayland.enable = true; # Xwayland can be disabled.
  # };

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;

  # Enable OpenGL
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.amdgpu.overdrive.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.nikita = {
    isNormalUser = true;
    description = "Nikita";
    extraGroups = [
      "networkmanager"
      "wheel"
      "ntsync"
    ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      #  thunderbird
    ];
  };
  users.groups.ntsync = { };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = commonPkgs.commonPackages ++ commonPkgs.nixosPackages;
    variables = {
      TERMINAL = "ghostty";
    };
    shellAliases = {
      update = "nix flake update --flake ~/nix/.; sudo nixos-rebuild switch --flake ~/nix/.";
      gridup = "rlwrap ~/9pro/9gc nikita";
    };
  };

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      noto-fonts-color-emoji
      # berkeley-mono
      (berkeley-mono.overrideAttrs (o: {
        nativeBuildInputs = [ pkgs.nerd-font-patcher ];
        postInstall = ''
          mkdir -p $out/share/fonts/truetype/{berkeley-mono,berkeley-mono-nerd}
          mv $out/share/fonts/truetype/BerkeleyMono-*.ttf $out/share/fonts/truetype/berkeley-mono/
          for f in $out/share/fonts/truetype/berkeley-mono/*.ttf; do
            nerd-font-patcher --complete --outputdir $out/share/fonts/truetype/berkeley-mono-nerd/ $f
          done
        '';
      }))
    ];
  };

  # Use latest nix binary
  nix.package = pkgs.nixVersions.latest;

  #programs.steam = {
  #enable = true;
  # gamescopeSession.enable = true;
  # doesnt work for some reason, using protonup instead
  # extraCompatPackages = [ pkgs.proton-ge-bin ];
  #};

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.emacs = {
    package = pkgs.emacs-gtk;
    enable = true;
  };

  # List services that you want to enable:
  systemd.packages = with pkgs; [ lact ];
  systemd.services.lactd.wantedBy = [ "multi-user.target" ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      AllowUsers = [ "nikita" ];
      PermitRootLogin = "no";
    };
  };

  services.fail2ban.enable = true;

  services.flatpak.enable = true;

  # build takes forever & fails randomly
  # re-enable when u actually need it
  virtualisation.virtualbox.host = {
    enable = false;
    enableKvm = true;
    addNetworkInterface = false;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
