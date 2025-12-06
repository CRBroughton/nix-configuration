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
      init = {
        defaultBranch = "master";
      };
      gpg = {
        format = "ssh";
      };
    };
  };

  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "CRBroughton";
        email = "crbroughton@posteo.uk";
      };
    };
  };
}
