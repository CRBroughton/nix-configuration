-- Using no-neck-pain.nvim as it's closer to Emacs olivetti behavior
return {
  'shortcuts/no-neck-pain.nvim',
  version = '*',
  config = function()
    require('no-neck-pain').setup({
      width = 200, -- Match olivetti-body-width
    })

    -- C-M-z toggle (Alt is A in Neovim)
    vim.keymap.set('n', '<C-A-z>', '<Cmd>NoNeckPain<CR>', { silent = true, desc = 'Toggle focus mode' })
  end,
}
