-- lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Configure vim.opt and keybindings here
require("user.option")
require("user.keymap")

-- Plugins
require("lazy").setup({
  spec = {
    -- Theme
    --"mofiqul/dracula.nvim",
    --vim.cmd([[colorscheme dracula]])
    {
      "sho-87/kanagawa-paper.nvim",
      lazy = false,
      priority = 1000,
      config = function()
        vim.cmd([[colorscheme kanagawa-paper]])
      end
    },

    -- The status line
    {
      "nvim-lualine/lualine.nvim",
      lazy = false,
      priority = 999,
      config = function()
        require("lualine").setup()
      end
    },

    -- A simple file manager
    -- TODO: Needs some work to set up the keybindings
    { "theblob42/drex.nvim" },

    -- Shows key binding information
    { "folke/which-key.nvim" },

    -- Completions
    {
      "hrsh7th/nvim-cmp",
      dependencies = {
        { "hrsh7th/cmp-buffer" },
        { "hrsh7th/cmp-nvim-lsp" }
      },
      config = function()
        require("cmp_nvim_lsp").default_capabilities()
        require("cmp").setup({
          sources = require("cmp").config.sources({
            { name = "nvim_lsp",                priority = 1000 },
            { name = "nvim_lsp_signature_help", priority = 999 },
            --{ name = "luasnip", priority = 750 },
            { name = "buffer",                  priority = 500 },
            { name = "path",                    priority = 250 },
          })
        })
      end
    },

    -- Provides easy LSP configuration
    {
      "neovim/nvim-lspconfig",
      dependencies = {
        { "lukas-reineke/lsp-format.nvim" },
        { "hoffs/omnisharp-extended-lsp.nvim" }
      },
      config = function()
        require("lsp-format").setup {}
        require("lspconfig").lua_ls.setup {
          -- Tell lua lsp to ignore vim global
          settings = {
            Lua = {
              diagnostics = {
                globals = { 'vim' }
              }
            }
          }
          --,on_attach = require("lsp-format").on_attach
        }
        require("lspconfig").omnisharp.setup {
          cmd = { "omnisharp", "-lsp" },
        }
      end
    },

    -- Provides better syntax highlighting/movement
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = { "nvim-treesitter/nvim-treesitter-context" },
    },

    -- Provides navigation and search pop up windows
    {
      "nvim-telescope/telescope.nvim",
      dependencies = { "nvim-lua/plenary.nvim" }
    },

    -- Auto indent on new line
    { "lukas-reineke/indent-blankline.nvim" },

    -- Markdown folding
    { "masukomi/vim-markdown-folding" },
  },
  checker = { enabled = true }
})
