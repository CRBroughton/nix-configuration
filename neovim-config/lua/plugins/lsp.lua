return {
  'neovim/nvim-lspconfig',
  dependencies = {
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'WhoIsSethDaniel/mason-tool-installer.nvim',
  },
  config = function()
    require('mason').setup()

    -- Auto-install LSP servers
    require('mason-lspconfig').setup({
      ensure_installed = { 'lua_ls', 'ts_ls', 'vue_ls', 'eslint', 'tailwindcss', 'unocss' },
    })

    -- Auto-install formatters/linters (non-LSP tools)
    require('mason-tool-installer').setup({
      ensure_installed = {
        'eslint_d', -- Fast ESLint daemon for formatting
      },
    })

    local capabilities = require('cmp_nvim_lsp').default_capabilities()

    -- Get vue-language-server path from Mason for TypeScript plugin
    local mason_packages = vim.fn.stdpath('data') .. '/mason/packages'
    local vue_language_server_path = mason_packages .. '/vue-language-server/node_modules/@vue/language-server'

    -- Lua
    vim.lsp.config('lua_ls', {
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' },
          },
        },
      },
    })

    -- TypeScript with Vue plugin (Hybrid mode for Volar 2.0+)
    vim.lsp.config('ts_ls', {
      capabilities = capabilities,
      filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue' },
      init_options = {
        plugins = {
          {
            name = '@vue/typescript-plugin',
            location = vue_language_server_path,
            languages = { 'vue' },
          },
        },
      },
    })

    -- Vue Language Server (handles CSS/HTML in .vue files)
    vim.lsp.config('vue_ls', {
      capabilities = capabilities,
      handlers = {
        ['tsserver/request'] = function(_, result, ctx)
          local clients = vim.lsp.get_clients({ name = 'ts_ls' })
          if #clients > 0 then
            clients[1]:request('tsserver/request', result, function(_, res)
              vim.lsp.get_client_by_id(ctx.client_id):notify('tsserver/response', res)
            end)
          end
        end,
      },
    })

    -- ESLint
    vim.lsp.config('eslint', {
      capabilities = capabilities,
      settings = {
        workingDirectories = { mode = 'auto' },
      },
    })

    -- Tailwind CSS
    vim.lsp.config('tailwindcss', {
      capabilities = capabilities,
      filetypes = { 'html', 'css', 'scss', 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'vue' },
    })

    -- UnoCSS
    vim.lsp.config('unocss', {
      capabilities = capabilities,
      filetypes = { 'html', 'css', 'scss', 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'vue' },
    })

    -- Enable the configured servers
    vim.lsp.enable({ 'lua_ls', 'ts_ls', 'vue_ls', 'eslint', 'tailwindcss', 'unocss' })

    -- LSP keymaps (matching Emacs M-. and M-,)
    vim.api.nvim_create_autocmd('LspAttach', {
      callback = function(args)
        local opts = { buffer = args.buf }
        vim.keymap.set('n', '<A-.>', vim.lsp.buf.definition, opts) -- M-. Go to definition
        vim.keymap.set('n', '<A-,>', '<C-o>', opts)                -- M-, Go back
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
      end,
    })
  end,
}
