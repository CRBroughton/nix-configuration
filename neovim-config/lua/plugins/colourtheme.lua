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
  end
}
