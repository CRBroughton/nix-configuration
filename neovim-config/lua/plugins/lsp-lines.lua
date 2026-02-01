return {
  'https://git.sr.ht/~whynothugo/lsp_lines.nvim',
  event = 'LspAttach',
  config = function()
    require('lsp_lines').setup()

    -- Disable default virtual_text since lsp_lines replaces it
    vim.diagnostic.config({
      virtual_text = false,
      virtual_lines = true,
      update_in_insert = true,
    })

    -- Toggle between lsp_lines and default virtual_text
    vim.keymap.set('n', '<leader>tl', function()
      local config = vim.diagnostic.config()
      if config.virtual_lines then
        vim.diagnostic.config({
          virtual_lines = false,
          virtual_text = true,
        })
      else
        vim.diagnostic.config({
          virtual_lines = true,
          virtual_text = false,
        })
      end
    end, { desc = 'Toggle lsp_lines' })
  end,
}
