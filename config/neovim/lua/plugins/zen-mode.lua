-- no-neck-pain.nvim: Centers the buffer for distraction-free writing/coding
return {
  'shortcuts/no-neck-pain.nvim',
  version = '*',
  config = function()
    require('no-neck-pain').setup({
      width = 100, -- Match olivetti-body-width
    })

    -- C-M-z toggle (Alt is A in Neovim)
    vim.keymap.set('n', '<C-A-z>', '<Cmd>NoNeckPain<CR>', { silent = true, desc = 'Toggle focus mode' })
  end,
}
