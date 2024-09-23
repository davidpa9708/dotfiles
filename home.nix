{ config, pkgs, pkgs-unstable, lib, ... }:
let
  pkgs-unstable = import <nixpkgs-unstable> { };
  pkgs-23-11 = import <nixos-23.11> { };
in {
  home = {
    username = "david";
    homeDirectory = "/home/david";
    stateVersion = "23.11";

    packages = (with pkgs; [
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
      nixfmt-classic
      syncthing
      slack
      thunderbird
      vscodium
      ungoogled-chromium
      vlc
      protonvpn-cli_2
      mongodb-compass
      libreoffice-qt
      tome4 # game
      feishin # spotify client
      spotify
      # standardnotes
      librewolf
      libsForQt5.polonium
      # godot stuff:
      gdtoolkit
      godot_4
      #media:
      inkscape
      blender
      gimp
      krita
      obs-studio
      fd
      ripgrep
    ]) ++ (with pkgs-unstable;
      [
        proton-pass # protonmail-desktop
      ]) ++ (with pkgs-23-11; [ standardnotes ])
      ++ (with pkgs.nodePackages_latest; [
        npm-check-updates
        prettier
        typescript
        typescript-language-server
        vscode-langservers-extracted
        yaml-language-server
        bash-language-server
        dockerfile-language-server-nodejs
        pyright
      ]);

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
      extraConfig = { core.sshCommand = "ssh -i ~/.ssh/personal"; };
      includes = [{
        condition = "gitdir:~/projects/inpt/";
        contents.user.name = "David";
        contents.user.email = "dav.perez@combocurve.com";
        contents.core.sshCommand = "ssh -i ~/.ssh/id_ed25519";
      }];
    };
    ssh = {
      enable = true;
      addKeysToAgent = "yes";
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      #enableBashIntegration = true; # see note on other shells below
      #enableFishIntegration = true;
    };
    home-manager.enable = true;
  };

  nixpkgs.config.permittedInsecurePackages =
    [ "electron-25.9.0" "electron-24.8.6" ]; # logseq feishin
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "slack"
      "mongodb-compass"
      "spotify"
      "keymapp"
    ];
}
