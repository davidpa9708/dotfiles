{ config, pkgs, pkgs-unstable, lib, buildPythonPackage, ... }:
let
  pkgs-unstable = import <nixpkgs-unstable> { };
  pkgs-23-11 = import <nixos-23.11> { };
in {
  home = {
    username = "david";
    homeDirectory = "/home/david";
    stateVersion = "23.11";

    packages = (with pkgs; [
      ## essentials
      starship
      git
      # syncthing
      ranger

      ## editors
      emacs29
      micro

      ## work
      vscodium
      slack
      mongodb-compass

      ## apps
      signal-desktop
      firefox
      libreoffice-qt
      qutebrowser
      keepassxc
      logseq
      thunderbird
      ungoogled-chromium
      vlc
      freetube
      spotify
      # feishin # spotify client

      tome4 # game

      ## creating media:
      inkscape
      blender
      gimp
      krita
      obs-studio

      ## others
      # libsForQt5.polonium
      nixfmt-classic
      protonvpn-cli_2
      fd
      ripgrep
      zeal
      wakatime

      ## hyprland
      libnotify
      kitty
      swaynotificationcenter
      kdePackages.polkit-kde-agent-1
      pipewire
      wireplumber
      playerctl
      tofi # wofi
      # waybar
      # hyprpaper
      eww
      wmctrl
      networkmanagerapplet
      flameshot
    ]) ++ (with pkgs-unstable;
      [
        proton-pass # protonmail-desktop
      ]) ++ (with pkgs-23-11; [ standardnotes ])
      ++ (with pkgs-unstable.nodePackages_latest;
        [
          npm-check-updates
          prettier
          typescript
          typescript-language-server
          vscode-langservers-extracted
          yaml-language-server
          bash-language-server
          dockerfile-language-server-nodejs
        ]

        ++ (with pkgs-unstable; [
          ## godot stuff:
          gdtoolkit_4
          godot_4
        ])

      );

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
    file = {
      # eww = {
      #   source = builtins.toPath ./eww;
      #   target = ".config/eww";
      # };
      # emacs = {
      #   source = builtins.toPath ./emacs;
      #   target = ".emacs.d";
      # };
    };

    sessionVariables = { EDITOR = "emacs"; };

    activation = {
      myActivationAction = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./.
        } $HOME/.config/home-manager/
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./emacs
        } $HOME/.emacs.d
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./.spacemacs
        } $HOME/.spacemacs
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./eww
        } $HOME/.config/eww
      '';
    };

  };

  programs = {
    home-manager.enable = true;
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
    librewolf = {
      enable = true;
      settings = {
        "privacy.resistFingerprinting.letterboxing" = true;
        "webgl.disabled" = false;
        "privacy.clearOnShutdown.history" = false;
        "privacy.clearOnShutdown.downloads" = false;
        "identity.fxaccounts.enabled" = true;
        "middlemouse.paste" = false;
      };
    };
  };

  nixpkgs.config.permittedInsecurePackages =
    [ "electron-25.9.0" "electron-24.8.6" "electron-27.3.11" ]; # logseq feishin
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "slack"
      "mongodb-compass"
      "spotify"
      "keymapp"
    ];

  # https://wiki.hyprland.org/Nix/Hyprland-on-Home-Manager/
  home.sessionVariables.NIXOS_OZONE_WL = "1";
  # https://wiki.hyprland.org/Nix/Hyprland-on-Home-Manager/
  home.pointerCursor = {
    gtk.enable = true;
    # x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };
  gtk = {
    enable = true;

    theme = {
      package = pkgs.flat-remix-gtk;
      name = "Flat-Remix-GTK-Grey-Darkest";
    };

    iconTheme = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
    };

    font = {
      name = "Sans";
      size = 11;
    };
  };
}
