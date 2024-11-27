{ config, lib, pkgs, ... }: {

  services.kanata = {
    enable = true;
    keyboards.david.configFile = ./kanata.kbd;
  };

}
