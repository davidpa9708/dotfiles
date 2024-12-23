{ config, lib, pkgs, ... }: {

  imports = [
    <nixos-hardware/lenovo/thinkpad/p14s>
    ./nixos/nvidia.nix
    ./nixos/kanata.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  time.timeZone = "America/Phoenix";

  programs.nix-ld.enable = true;

  # Sets up all the libraries to load
  # programs.nix-ld.libraries = with pkgs; [ stdenv.cc.cc openssl ];

  services.ntp.enable = true;
  services.automatic-timezoned.enable = true;

  services.xserver.enable = true;
  # services.desktopManager.plasma5.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  fonts.packages = with pkgs; [ source-code-pro ];

  programs.hyprland.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.settings = {
    General = { Enable = "Source,Sink,Media,Socket"; };
  };
  hardware.bluetooth.powerOnBoot =
    true; # powers up the default Bluetooth controller on boot

  sound.mediaKeys.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  #users.defaultUserShell = pkgs.fish;
  users.users.david.shell = pkgs.fish;
  users.users.root.extraGroups = [ "uinput" "docker" ];
  users.users.david.extraGroups = [ "uinput" "docker" ];
  programs.fish.enable = true;
  #programs.bash.enable = true;

  hardware.keyboard.zsa.enable = true;

  # hardware.bumblebee.driver = "nouveau";

  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };
  services.traefik.group = "docker";
  boot.kernelPackages = pkgs.linuxPackages;
  # pkgs.linuxPackages_latest;
  programs.partition-manager.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "nvidia-x11"
      "nvidia-settings"
      "steam"
      "steam-original"
      "steam-run"
    ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall =
      true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

}
