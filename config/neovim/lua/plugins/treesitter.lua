-- nvim-treesitter: Syntax highlighting and code understanding via tree-sitter parsers
return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  opts = {
    ensure_installed = { 'lua', 'vim', 'vimdoc', 'typescript', 'javascript', 'html', 'css', 'json', 'markdown', 'nix', 'odin' },
    highlight = { enable = true },
  },
}
