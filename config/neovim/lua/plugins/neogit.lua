-- neogit: Git interface for staging, committing, pushing, pulling, branching, and viewing logs
return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'sindrets/diffview.nvim',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
    local neogit = require('neogit')
    neogit.setup({
      integrations = {
        diffview = true,
        telescope = true,
      },
      -- Use default mappings - they're already similar to magit:
      -- s = stage, u = unstage, c = commit, p = pull, P = push
      -- b = branch, l = log, d = diff, q = quit
    })

    -- C-x g for git status (Emacs magit binding)
    vim.keymap.set('n', '<C-x>g', '<Cmd>Neogit<CR>', { silent = true, desc = 'Git status (Neogit)' })
  end,
}
