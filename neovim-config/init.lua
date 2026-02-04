require 'core.keymaps'
require 'core.options'
require 'core.autocmds'

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
  if vim.v.shell_error ~= 0 then
    error('Error cloning lazy.nvim:\n' .. out)
  end
end

---@type vim.Option
local rtp = vim.opt.rtp
rtp:prepend(lazypath)

require('lazy').setup({
  -- Existing plugins
  require 'plugins.colourtheme',
  require 'plugins.neotree',

  -- Navigation & Search (ivy/counsel equivalent)
  require 'plugins.telescope',

  -- Completion (company equivalent)
  require 'plugins.completion',

  -- UI
  require 'plugins.lualine',
  require 'plugins.bufferline',
  require 'plugins.which-key',
  require 'plugins.rainbow-delimiters',
  require 'plugins.zen-mode',

  -- Git (magit equivalent)
  require 'plugins.neogit',
  require 'plugins.conventional-commit',

  -- Language support
  require 'plugins.treesitter',
  require 'plugins.lsp',
  require 'plugins.formatting',

  -- Editor enhancements
  require 'plugins.gitsigns',
  require 'plugins.autopairs',
  require 'plugins.lsp-lines',
  require 'plugins.autotag',
  require 'plugins.colorizer',
  require 'plugins.persistence',
  require 'plugins.claude',
  require 'plugins.vimbegood',
  require 'plugins.lazygit',
  require 'plugins.lazydocker',
  require 'plugins.twilight',
})
