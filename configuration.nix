{ config, lib, pkgs, ... }:
{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd"; 

  # time.timeZone = "Europe/Amsterdam";

  services.ntp.enable = true;

  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  
  programs.hyprland.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.keyboard.zsa.enable = true;
  
  sound.mediaKeys.enable = true;
    
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  #users.defaultUserShell = pkgs.fish;
  users.users.david.shell = pkgs.fish;
  programs.fish.enable = true;
  #programs.bash.enable = true;

  hardware.bumblebee.driver = "nouveau";
}
