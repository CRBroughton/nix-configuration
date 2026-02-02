-- neo-tree.nvim: File explorer sidebar with git status integration
return {
  'nvim-neo-tree/neo-tree.nvim',
  branch = 'v3.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
    'nvim-tree/nvim-web-devicons',
  },
  lazy = false,
  config = function()
    require('neo-tree').setup({
      close_if_last_window = true,
      window = {
        position = 'left',
        width = 30,
      },
      filesystem = {
        follow_current_file = {
          enabled = true,
        },
        use_libuv_file_watcher = true,
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
        },
      },
      default_component_configs = {
        git_status = {
          symbols = {
            added = '+',
            modified = '~',
            deleted = '-',
            renamed = 'r',
            untracked = '?',
            ignored = '!',
            unstaged = 'U',
            staged = 'S',
            conflict = 'C',
          },
        },
      },
    })

    -- Toggle with C-b (like VSCode)
    vim.keymap.set('n', '<C-b>', '<Cmd>Neotree toggle<CR>', { desc = 'Toggle file explorer' })
  end,
}
