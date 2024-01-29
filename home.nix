{ config, pkgs, lib, ... }: {

  home = {
    username = "david";
    homeDirectory = "/home/david";
    stateVersion = "23.11";

    packages = with pkgs; [
      emacs29
      #emacs29-nox
      git
      micro
      signal-desktop
      firefox
      keepassxc
      logseq
      starship
      # android-tools
      nixfmt
      syncthing
      slack
      thunderbird
      vscodium
      godot_4
      ungoogled-chromium
      gimp
      blender
      obs-studio
      vlc
      protonvpn-cli_2
    ];

    shellAliases = {
      ne = "sudo -E emacs ~/dotfiles/configuration.nix";
      nr = "sudo nixos-rebuild switch";
      he = "emacs ~/dotfiles/home.nix";
      hr = "home-manager switch";
    };

    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    # Home Manager is pretty good at managing dotfiles. The primary way to manage
    # plain files is through 'home.file'.
    file = { };

    sessionVariables = { EDITOR = "emacs"; };

    activation = {
      myActivationAction = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./emacs
        } $HOME/.emacs.d/
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./.
        } $HOME/.config/home-manager/
      '';
    };

  };

  programs = {
    bash.enable = true;
    fish.enable = true;
    starship.enable = true;
    starship.enableFishIntegration = true;
    git = {
      enable = true;
      userName = "David Perez Alvarez";
      userEmail = "david@leddgroup.com";
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      #enableBashIntegration = true; # see note on other shells below
      #enableFishIntegration = true;
    };
    home-manager.enable = true;
  };

  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ]; # logseq
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "slack" ];
}
