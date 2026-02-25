{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    signing = {
      key = "~/.ssh/id_ed25519";
      signByDefault = true;
    };
    settings = {
      user = {
        name = "CRBroughton";
        email = "crbroughton@posteo.uk";
      };
      init.defaultBranch = "master";
      gpg.format = "ssh";
    };
  };

  programs.jujutsu = {
    enable = true;
    settings.user = {
      name = "CRBroughton";
      email = "crbroughton@posteo.uk";
    };
  };

  # SSH configuration
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*".identityFile = "~/.ssh/id_ed25519";
  };

  services.ssh-agent.enable = true;

  # SSH public key
  home.file.".ssh/id_ed25519.pub".text = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk
  '';

  # Fix SSH key permissions
  home.activation.fixSshPermissions = {
    after = [ "writeBoundary" ];
    before = [ ];
    data = ''
      if [ -f ~/.ssh/id_ed25519 ]; then
        chmod 600 ~/.ssh/id_ed25519
      fi
      if [ -d ~/.ssh ]; then
        chmod 700 ~/.ssh
      fi
    '';
  };

  home.packages = with pkgs; [
    lazygit
  ];
}
