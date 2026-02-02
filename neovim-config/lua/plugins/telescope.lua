return {
  'nvim-telescope/telescope.nvim',
  branch = 'master', -- Use master branch for Neovim 0.11+ treesitter compatibility (PR #3566)
  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  },
  config = function()
    local telescope = require('telescope')
    local actions = require('telescope.actions')

    telescope.setup({
      defaults = {
        mappings = {
          i = {
            ['<C-j>'] = actions.move_selection_next,
            ['<C-k>'] = actions.move_selection_previous,
            ['<Tab>'] = actions.select_default,
            ['<C-l>'] = actions.select_default,
          },
          n = {
            ['<C-j>'] = actions.move_selection_next,
            ['<C-k>'] = actions.move_selection_previous,
          },
        },
      },
    })

    -- Load fzf extension (requires compilation - needs gcc and make)
    pcall(telescope.load_extension, 'fzf')

    -- Keymaps (ivy/counsel equivalents)
    local builtin = require('telescope.builtin')
    vim.keymap.set('n', '<C-p>', builtin.find_files, { desc = 'Find file in project' })
    vim.keymap.set('n', '<C-f>', builtin.current_buffer_fuzzy_find, { desc = 'Find in buffer' })
    vim.keymap.set('n', '<A-x>', builtin.commands, { desc = 'Command palette' })
    vim.keymap.set('n', '<C-x>b', builtin.buffers, { desc = 'Switch buffer' })
  end,
}
