{ config, pkgs, ... }:
{
  
  home = {
    username = "david";
    homeDirectory = "/home/david";
    stateVersion = "23.11";
    
    packages = with pkgs; [
      
      emacs29
      git
      micro
      signal-desktop
      firefox
      keepassxc
      logseq
      starship
      # android-tools
      
    ];
     
    shellAliases = {
      ne = "sudo -E emacs /etc/nixos/configuration.nix";
      nr = "sudo nixos-rebuild switch";     
    };
    
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    
    
    # Home Manager is pretty good at managing dotfiles. The primary way to manage
    # plain files is through 'home.file'.
    file = {
      # # Building this configuration will create a copy of 'dotfiles/screenrc' in
      # # the Nix store. Activating the configuration will then make '~/.screenrc' a
      # # symlink to the Nix store copy.
      # ".screenrc".source = dotfiles/screenrc;
        
      # # You can also set the file content immediately.
      # ".gradle/gradle.properties".text = ''
      #   org.gradle.console=verbose
      #   org.gradle.daemon.idletimeout=3600000
      # '';
    };
    
    sessionVariables = {
      # EDITOR = "emacs";
    };
  };
  
  programs = {
    fish.enable = true;
    starship.enable = true;
    starship.enableFishIntegration = true;
    git = {
      enable = true;
      userName = "David Perez Alvarez";
      userEmail = "david@leddgroup.com";
    };
    #xsession.enable = true;
    home-manager.enable = true;  
  };

  
  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/david/etc/profile.d/hm-session-vars.sh
  #

  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
  ];

}
