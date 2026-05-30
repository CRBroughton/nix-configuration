return {
  "akinsho/toggleterm.nvim",
  keys = {
    {
      "<leader>jj",
      function()
        require("toggleterm.terminal").Terminal:new({
          cmd = "jjui",
          direction = "float",
          float_opts = {
            border = "curved",
            width = math.floor(vim.o.columns * 0.9),
            height = math.floor(vim.o.lines * 0.9),
          },
          on_open = function(term)
            vim.keymap.set("t", "q", function() term:close() end, { buffer = term.bufnr })
          end,
        }):toggle()
      end,
      desc = "jjui",
    },
  },
}
