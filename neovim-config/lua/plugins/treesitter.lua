-- nvim-treesitter: Syntax highlighting and code understanding via tree-sitter parsers
return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  config = function()
    -- Install parsers via command - run :TSInstall <lang> manually if needed
    -- Or use :TSInstall lua vim vimdoc typescript javascript html css json markdown nix
  end,
}
