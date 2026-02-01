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

    -- Buffer navigation
    vim.keymap.set('n', '<C-Left>', '<Cmd>BufferLineCyclePrev<CR>', { silent = true, desc = 'Previous buffer' })
    vim.keymap.set('n', '<C-Right>', '<Cmd>BufferLineCycleNext<CR>', { silent = true, desc = 'Next buffer' })
  end,
}
