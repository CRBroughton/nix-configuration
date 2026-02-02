-- persistence.nvim: Automatically saves and restores sessions per directory
return {
  'folke/persistence.nvim',
  lazy = false, -- Load immediately
  config = function()
    require('persistence').setup({
      dir = vim.fn.stdpath('state') .. '/sessions/',
      need = 1,
      branch = true,
    })

    -- Save session on exit
    vim.api.nvim_create_autocmd('VimLeavePre', {
      callback = function()
        require('persistence').save()
      end,
    })

    -- Restore last session for current directory
    vim.api.nvim_create_autocmd('VimEnter', {
      callback = function()
        -- Only restore if no files were passed as arguments
        if vim.fn.argc() == 0 and not vim.g.started_with_stdin then
          -- Delay slightly to let other plugins initialize
          vim.defer_fn(function()
            require('persistence').load()
          end, 50)
        end
      end,
      nested = true,
    })
  end,
}
