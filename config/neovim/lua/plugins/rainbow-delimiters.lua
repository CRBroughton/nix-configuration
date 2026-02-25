-- rainbow-delimiters.nvim: Colorizes matching brackets/parentheses for easier visual pairing
return {
  'HiPhish/rainbow-delimiters.nvim',
  event = 'BufReadPost',
  config = function()
    require('rainbow-delimiters.setup').setup({})
  end,
}
