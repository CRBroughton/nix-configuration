{ pkgs, ... }:

{
  home.packages = with pkgs; [
    neovim
  ];

  home.file.".config/nvim/init.lua" = {
    source = ../neovim-config/init.lua;
  };

  home.file.".config/nvim/lua" = {
    source = ../neovim-config/lua;
    recursive = true;
  };
}
