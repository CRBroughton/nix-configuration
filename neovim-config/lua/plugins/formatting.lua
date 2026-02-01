return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  config = function()
    require('conform').setup({
      formatters_by_ft = {
        javascript = { 'eslint_d' },
        typescript = { 'eslint_d' },
        javascriptreact = { 'eslint_d' },
        typescriptreact = { 'eslint_d' },
        vue = { 'eslint_d' },
        json = { 'eslint_d' },
        go = { 'goimports' },
      },
      format_on_save = {
        timeout_ms = 3000,
        lsp_fallback = true,
      },
    })
  end,
}
