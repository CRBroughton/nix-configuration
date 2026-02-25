-- Disable line numbers in terminal buffers (matching Emacs ui.el)
vim.api.nvim_create_autocmd('TermOpen', {
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})

-- Auto-enter insert mode in terminal
vim.api.nvim_create_autocmd('TermOpen', {
  callback = function()
    vim.cmd('startinsert')
  end,
})

-- Close terminal with Escape in terminal mode
vim.api.nvim_create_autocmd('TermOpen', {
  callback = function()
    vim.keymap.set('t', '<Esc>', '<C-\\><C-n><Cmd>bdelete!<CR>', { buffer = true })
  end,
})
