-- Leader key configuration
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- ============================================================================
-- GENERAL (matching Emacs keybindings.el)
-- ============================================================================

-- Save with C-s
vim.keymap.set({ 'n', 'i' }, '<C-s>', '<Cmd>w<CR>', { desc = 'Save buffer' })

-- Undo with C-z
vim.keymap.set('n', '<C-z>', 'u', { desc = 'Undo' })
vim.keymap.set('i', '<C-z>', '<C-o>u', { desc = 'Undo' })

-- Split window right and switch (C-\)
vim.keymap.set('n', '<C-\\>', '<Cmd>vsplit<CR><C-w>l', { desc = 'Split right and switch' })

-- Reload config (F5)
vim.keymap.set('n', '<F5>', '<Cmd>source $MYVIMRC<CR><Cmd>echo "Config reloaded!"<CR>', { desc = 'Reload config' })

-- Window navigation (M-PageUp/Down)
vim.keymap.set('n', '<A-PageUp>', '<C-w>W', { desc = 'Previous window' })
vim.keymap.set('n', '<A-PageDown>', '<C-w>w', { desc = 'Next window' })

-- ============================================================================
-- SMART ESCAPE
-- ============================================================================

local function smart_escape()
  local buftype = vim.bo.buftype
  local filetype = vim.bo.filetype

  -- Terminal mode: close buffer and window
  if buftype == 'terminal' then
    vim.cmd('bdelete!')
    if #vim.api.nvim_list_wins() > 1 then
      vim.cmd('close')
    end
    return
  end

  -- Special buffers (neo-tree, neogit, etc): just close
  if buftype == 'nofile' or filetype == 'neo-tree' or filetype == 'NeogitStatus' or filetype == 'TelescopePrompt' then
    vim.cmd('close')
    return
  end

  -- Check if buffer is modified
  if vim.bo.modified then
    vim.notify('Buffer has unsaved changes. Save with C-s or use :q! to force close.', vim.log.levels.WARN)
    return
  end

  -- Regular buffer: close buffer, or window if last buffer
  local buffers = vim.fn.getbufinfo({ buflisted = 1 })
  if #buffers > 1 then
    vim.cmd('bdelete')
  elseif #vim.api.nvim_list_wins() > 1 then
    vim.cmd('close')
  else
    vim.cmd('bdelete')
  end
end

vim.keymap.set('n', '<Esc>', smart_escape, { desc = 'Smart escape' })

-- ============================================================================
-- TERMINAL (C-t)
-- ============================================================================

local function open_terminal_below()
  vim.cmd('below split | resize 15 | terminal')
  vim.cmd('startinsert')
end

vim.keymap.set('n', '<C-t>', open_terminal_below, { desc = 'Open terminal below' })

-- ============================================================================
-- KEYBINDINGS HELP (C-?)
-- ============================================================================

local function show_keybindings()
  local lines = {
    '╔══════════════════════════════════════════════════════════════╗',
    '║                    CUSTOM KEYBINDINGS                        ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  GENERAL                                                     ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  C-s          Save buffer                                    ║',
    '║  C-z          Undo                                           ║',
    '║  ESC          Close buffer/tab/window (smart)                ║',
    '║  F5           Reload config                                  ║',
    '║  C-?          Show this help                                 ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  FILES & SEARCH                                              ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  C-p          Find file in project (fuzzy)                   ║',
    '║  C-f          Find in buffer                                 ║',
    '║  A-x          Command palette                                ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  WINDOWS & TABS                                              ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  C-\\          Split window right                             ║',
    '║  C-t          Open terminal below                            ║',
    '║  C-PageUp     Previous tab                                   ║',
    '║  C-PageDown   Next tab                                       ║',
    '║  A-PageUp     Previous window                                ║',
    '║  A-PageDown   Next window                                    ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  GIT                                                         ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  C-x g        Neogit status                                  ║',
    '║  C-x c        Conventional commit                            ║',
    '║                                                              ║',
    '║  In Neogit status (C-x g):                                   ║',
    '║  s            Stage                                          ║',
    '║  u            Unstage                                        ║',
    '║  c            Commit                                         ║',
    '║  p            Pull                                           ║',
    '║  P            Push                                           ║',
    '║  b            Branch                                         ║',
    '║  l            Log                                            ║',
    '║  q            Quit                                           ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  CODE NAVIGATION                                             ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  A-.          Go to definition                               ║',
    '║  A-,          Go back                                        ║',
    '║  K            Hover documentation                            ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  MODES                                                       ║',
    '╠══════════════════════════════════════════════════════════════╣',
    '║  C-A-z        Toggle focus mode                              ║',
    '╚══════════════════════════════════════════════════════════════╝',
  }

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local width = 66
  local height = #lines
  vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    width = width,
    height = height,
    col = (vim.o.columns - width) / 2,
    row = (vim.o.lines - height) / 2,
    style = 'minimal',
    border = 'rounded',
  })

  vim.keymap.set('n', 'q', '<Cmd>close<CR>', { buffer = buf })
  vim.keymap.set('n', '<Esc>', '<Cmd>close<CR>', { buffer = buf })
end

vim.keymap.set('n', '<C-?>', show_keybindings, { desc = 'Show keybindings' })
