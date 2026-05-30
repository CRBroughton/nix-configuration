return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  config = function()
    require("catppuccin").setup({
      flavour = "mocha",
      integrations = {
        treesitter = true,
        native_lsp = { enabled = true },
        mason = true,
        telescope = { enabled = true },
        neotree = true,
        bufferline = true,
        gitsigns = true,
        which_key = true,
      },
    })
    vim.cmd.colorscheme("catppuccin")
  end,
}
