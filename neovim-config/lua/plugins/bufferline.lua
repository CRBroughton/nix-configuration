return {
  'akinsho/bufferline.nvim',
  version = '*',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require('bufferline').setup({
      options = {
        mode = 'buffers',
        show_buffer_close_icons = false,
        show_close_icon = false,
        separator_style = 'thin',
      },
    })

    -- Tab navigation (Emacs C-PageUp/Down)
    vim.keymap.set('n', '<C-PageUp>', '<Cmd>BufferLineCyclePrev<CR>', { silent = true, desc = 'Previous tab' })
    vim.keymap.set('n', '<C-PageDown>', '<Cmd>BufferLineCycleNext<CR>', { silent = true, desc = 'Next tab' })
  end,
}
