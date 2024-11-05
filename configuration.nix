{ config, lib, pkgs, ... }: {

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  # time.timeZone = "Europe/Amsterdam";

  programs.nix-ld.enable = true;

  services.ntp.enable = true;

  services.xserver.enable = true;
  # services.desktopManager.plasma5.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  fonts.packages = with pkgs; [ source-code-pro ];

  services.kanata = {
    enable = true;
    keyboards.david.configFile = ./kanata.kbd;
  };

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
  boot.kernelPackages = pkgs.linuxPackages_latest;
  programs.partition-manager.enable = true;

  hardware.opengl = { enable = true; };
  # Load nvidia driver for Xorg and Wayland
  # services.xserver.videoDrivers = [ "nvidia" ];

  # hardware.nvidia = {

  #   # Modesetting is required.
  #   modesetting.enable = true;

  #   # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
  #   # Enable this if you have graphical corruption issues or application crashes after waking
  #   # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead 
  #   # of just the bare essentials.
  #   powerManagement.enable = false;

  #   # Fine-grained power management. Turns off GPU when not in use.
  #   # Experimental and only works on modern Nvidia GPUs (Turing or newer).
  #   powerManagement.finegrained = false;

  #   # Use the NVidia open source kernel module (not to be confused with the
  #   # independent third-party "nouveau" open source driver).
  #   # Support is limited to the Turing and later architectures. Full list of 
  #   # supported GPUs is at: 
  #   # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
  #   # Only available from driver 515.43.04+
  #   # Currently alpha-quality/buggy, so false is currently the recommended setting.
  #   open = true;

  #   # Enable the Nvidia settings menu,
  #   # accessible via `nvidia-settings`.
  #   nvidiaSettings = true;

  #   # package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
  #   #   version = "560.35.03";
  #   #   sha256_64bit = "sha256-8pMskvrdQ8WyNBvkU/xPc/CtcYXCa7ekP73oGuKfH+M=";
  #   #   sha256_aarch64 = "sha256-8hyRiGB+m2hL3c9MDA/Pon+Xl6E788MZ50WrrAGUVuY=";
  #   #   openSha256 = "sha256-8hyRiGB+m2hL3c9MDA/Pon+Xl6E788MZ50WrrAGUVuY=";
  #   #   settingsSha256 = "sha256-ZpuVZybW6CFN/gz9rx+UJvQ715FZnAOYfHn5jt5Z2C8=";
  #   #   persistencedSha256 =
  #   #     "sha256-8pMskvrdQ8WyNBvkU/xPc/CtcYXCa7ekP73oGuKfH+M=";
  #   # };

  #   prime = {
  #     # Make sure to use the correct Bus ID values for your system!
  #     intelBusId = "PCI:0:2:0";
  #     nvidiaBusId = "PCI:1:0:0";
  #     # amdgpuBusId = "PCI:54:0:0"; For AMD GPU
  #   };
  # };

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      # "nvidia-x11"
      # "nvidia-settings"
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
