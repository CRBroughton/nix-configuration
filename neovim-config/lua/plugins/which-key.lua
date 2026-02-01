return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  config = function()
    local wk = require('which-key')
    wk.setup({
      delay = 0, -- Match Emacs which-key-idle-delay 0
    })

    -- Register key groups for Emacs-style prefixes
    wk.add({
      { '<C-x>', group = 'Emacs prefix' },
    })
  end,
}
