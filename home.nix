{ config, pkgs, pkgs-unstable, lib, buildPythonPackage, ... }:
let
  pkgs-unstable = import <home-manager> { };
  pkgs-23-11 = import <home-manager> { };
in {
  home = {
    username = "david";
    homeDirectory = "/home/david";
    stateVersion = "23.11";

    packages = (with pkgs; [
      ## essentials
      starship
      #git
      # syncthing
      #ranger

      ## editors
      # emacs-29
	  typescript-language-server
      #micro

      ## work
      #vscodium
      #slack
      #mongodb-compass

      ## apps
      #signal-desktop
      #firefox
      #libreoffice-qt
      #qutebrowser
      #keepassxc
      #logseq
      #thunderbird
      #ungoogled-chromium
      #vlc
      #freetube
      #spotify
      # feishin # spotify client

      #tome4 # game

      ## creating media:
      #inkscape
      #blender
      #gimp
      #krita
      #obs-studio

      ## others
      # libsForQt5.polonium
      #nixfmt-classic
      #protonvpn-cli_2
      fd
      ripgrep
      #zeal
      #wakatime
	  kanata

      ## hyprland
      #libnotify
      #kitty
      #swaynotificationcenter
      #kdePackages.polkit-kde-agent-1
      #pipewire
      #wireplumber
      #playerctl
      #tofi # wofi
      # waybar
      # hyprpaper
      #eww
      #wmctrl
      #networkmanagerapplet
      flameshot

	  # zed-editor
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

    sessionVariables = { EDITOR = "emacsclient -t"; };

    activation = {
      myActivationAction = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./.
        } $HOME/.config/home-manager
        $DRY_RUN_CMD ln -fs $VERBOSE_ARG ${
          builtins.toPath ./emacs.d
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
      package = pkgs.gitFull;
	  enable = true;
      userName = "David Perez Alvarez";
      userEmail = "david@leddgroup.com";
      extraConfig = {
	  	core.sshCommand = "ssh -i ~/.ssh/personal";
	  	credential.helper = "libsecret";
	  };
      includes = [{
        condition = "gitdir:~/projects/inpt/";
        contents.user.name = "David";
        contents.user.email = "dav.perez@combocurve.com";
        contents.core.sshCommand = "ssh -i ~/.ssh/work";
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
    ## librewolf = {
    ##   enable = true;
    ##   settings = {
    ##     "privacy.resistFingerprinting.letterboxing" = true;
    ##     "webgl.disabled" = false;
    ##     "privacy.clearOnShutdown.history" = false;
    ##     "privacy.clearOnShutdown.downloads" = false;
    ##     "identity.fxaccounts.enabled" = true;
    ##     "middlemouse.paste" = false;
    ##   };
    ## };
	zoxide = {
    	enable = true;
    	options = [ "--cmd" "j" ];
  	};
  };

  systemd.user = {
    enable = true;

	services = {
      # https://github.com/jtroo/kanata/discussions/130#discussioncomment-10227272
	  kanata = {
	    Unit = {
		  Description = "My NIX Kanata Service";
		  Requires="local-fs.target";
		  After="local-fs.target";
		};
        Service = {
		  ExecStartPre="/usr/sbin/modprobe uinput";
		  ExecStart="kanata -c /etc/kanata/kanata.kbd";
          Restart="no";
        };
        Install = {
		  WantedBy = [ "sysinit.target" ];
		};
	  };
	};
  };
}
