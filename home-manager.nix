{ config, pkgs, ... }:
{
  home.username = "david";
  home.homeDirectory = "/home/david";

  home.stateVersion = "23.11";

  programs.home-manager.enable = true;
}
