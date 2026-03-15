-- lualine.nvim: Fast statusline showing mode, git branch, diagnostics, and file info
return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require('lualine').setup({
      options = {
        theme = 'auto',
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { 'filename' },
        lualine_x = { 'encoding', 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
      },
    })

    -- Toggle statusline and command line visibility
    vim.keymap.set('n', '<leader>tb', function()
      if vim.o.laststatus == 0 then
        vim.o.laststatus = 3
        vim.o.cmdheight = 1
      else
        vim.o.laststatus = 0
        vim.o.cmdheight = 0
      end
    end, { desc = 'Toggle bottom bars' })
  end,
}
