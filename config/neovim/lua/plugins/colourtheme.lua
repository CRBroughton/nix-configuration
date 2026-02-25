return {
  '2nthony/vitesse.nvim',
  dependencies = {
    "tjdevries/colorbuddy.nvim"
  },
  lazy = false,
  priority = 1000,
  config = function()
    -- 1. Configure the theme options
    require("vitesse").setup({
      comment_italics = true,
      transparent_background = true,       -- Enables main transparency
      transparent_float_background = true, -- Enables transparency for popups (LSP, autocomplete)
      diagnostic_virtual_text_background = false,
    })

    -- 2. Set recommended vim options for transparency
    vim.opt.winblend = 0
    vim.opt.pumblend = 0

    -- 3. Finally, load the colorscheme
    vim.cmd.colorscheme('vitesse')

    -- base colour for unknown elements from the LSP
    vim.api.nvim_set_hl(0, "htmlTag", { fg = "#4d9375" })
    vim.api.nvim_set_hl(0, "htmlTagN", { fg = "#4d9375" })

    -- When volar knows this is a component, change the colour
    vim.api.nvim_set_hl(0, "@lsp.type.component.vue", { fg = "#b8a965", bold = true })
  end
}
