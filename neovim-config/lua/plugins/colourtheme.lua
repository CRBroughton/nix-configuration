return {
"2nthony/vitesse.nvim",
dependencies = {
    "tjdevries/colorbuddy.nvim"
},
lazy = false,
priority = 1000,
config = function()
    vim.cmd.colorscheme('vitesse')
end
}