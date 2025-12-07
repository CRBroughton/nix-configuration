{ config, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };

  services.ssh-agent.enable = true;

  home.file.".ssh/id_ed25519.pub" = {
    text = ''
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrDtLXrygEh0uessk5PifLw+t6SDKJz08w6u9iQxMpo crbroughton@posteo.uk
    '';
  };

  # Automatically fix SSH key permissions
  home.activation.fixSshPermissions = {
    after = [ "writeBoundary" ];
    before = [ ];
    data = ''
      if [ -f ~/.ssh/id_ed25519 ]; then
        chmod 600 ~/.ssh/id_ed25519
        echo "Fixed permissions for ~/.ssh/id_ed25519"
      fi

      if [ -d ~/.ssh ]; then
        chmod 700 ~/.ssh
      fi
    '';
  };
}
