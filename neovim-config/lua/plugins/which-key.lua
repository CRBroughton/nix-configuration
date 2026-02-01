return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  config = function()
    local wk = require('which-key')
    wk.setup({
      delay = 0,
      icons = {
        breadcrumb = '»',
        separator = '➜',
        group = '+',
      },
    })

    -- C-x prefix keybindings (main command menu)
    vim.keymap.set('n', '<C-x>s', '<Cmd>w<CR>', { desc = 'Save buffer' })
    vim.keymap.set('n', '<C-x>f', '<Cmd>Telescope find_files<CR>', { desc = 'Find file' })
    vim.keymap.set('n', '<C-x>/', '<Cmd>Telescope current_buffer_fuzzy_find<CR>', { desc = 'Find in buffer' })
    vim.keymap.set('n', '<C-x>b', '<Cmd>Neotree toggle<CR>', { desc = 'Toggle file explorer' })
    vim.keymap.set('n', '<C-x>t', function()
      vim.cmd('below split | resize 15 | terminal')
      vim.cmd('startinsert')
    end, { desc = 'Open terminal' })
    vim.keymap.set('n', '<C-x>\\', '<Cmd>vsplit<CR><C-w>l', { desc = 'Split right' })
    vim.keymap.set('n', '<C-x>0', '<Cmd>close<CR>', { desc = 'Close window' })
    vim.keymap.set('n', '<C-x>1', '<Cmd>only<CR>', { desc = 'Close other windows' })
    vim.keymap.set('n', '<C-x>?', '<Cmd>WhichKey<CR>', { desc = 'Show all keymaps' })

    -- Register key groups and descriptions
    wk.add({
      -- C-x prefix (main command menu)
      { '<C-x>', group = 'Commands' },
      { '<C-x>s', desc = 'Save buffer' },
      { '<C-x>f', desc = 'Find file' },
      { '<C-x>/', desc = 'Find in buffer' },
      { '<C-x>b', desc = 'Toggle file explorer' },
      { '<C-x>t', desc = 'Open terminal' },
      { '<C-x>\\', desc = 'Split right' },
      { '<C-x>0', desc = 'Close window' },
      { '<C-x>1', desc = 'Close other windows' },
      { '<C-x>g', desc = 'Neogit status' },
      { '<C-x>c', desc = 'Conventional commit' },
      { '<C-x>?', desc = 'Show all keymaps' },

      -- Leader prefix groups
      { '<leader>h', group = 'Git hunks' },
      { '<leader>hs', desc = 'Stage hunk' },
      { '<leader>hr', desc = 'Reset hunk' },
      { '<leader>hu', desc = 'Undo stage hunk' },
      { '<leader>hp', desc = 'Preview hunk' },
      { '<leader>hb', desc = 'Blame line' },

      { '<leader>r', group = 'Refactor' },
      { '<leader>rn', desc = 'Rename symbol' },

      { '<leader>c', group = 'Code' },
      { '<leader>ca', desc = 'Code action' },

      { '<leader>t', group = 'Toggle' },
      { '<leader>tl', desc = 'Toggle inline diagnostics' },

      -- Navigation hints
      { ']c', desc = 'Next git hunk' },
      { '[c', desc = 'Previous git hunk' },
      { 'gr', desc = 'Find references' },
      { 'K', desc = 'Hover documentation' },

      -- Direct keybindings (also work without C-x)
      { '<C-s>', desc = 'Save buffer' },
      { '<C-z>', desc = 'Undo' },
      { '<C-p>', desc = 'Find file' },
      { '<C-f>', desc = 'Find in buffer' },
      { '<C-b>', desc = 'Toggle file explorer' },
      { '<C-t>', desc = 'Open terminal' },
      { '<C-\\>', desc = 'Split right' },

      -- Alt key bindings
      { '<A-x>', desc = 'Command palette' },
      { '<A-.>', desc = 'Go to definition' },
      { '<A-,>', desc = 'Go back' },
      { '<A-Left>', desc = 'Previous window' },
      { '<A-Right>', desc = 'Next window' },

      -- Other
      { '<C-Left>', desc = 'Previous buffer' },
      { '<C-Right>', desc = 'Next buffer' },
      { '<C-A-z>', desc = 'Toggle focus mode' },
      { '<F5>', desc = 'Reload config' },
    })
  end,
}
