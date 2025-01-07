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
vim.g.markdown_folding = 1

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
        local cmp = require("cmp")
        cmp.setup({
          mapping = cmp.mapping.preset.insert({
            -- ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            -- ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true })
          }),
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
        { "hoffs/omnisharp-extended-lsp.nvim" }
      },
      config = function()
        require("lspconfig").lua_ls.setup {
          -- Tell lua lsp to ignore vim global
          settings = {
            Lua = {
              diagnostics = {
                globals = { 'vim' }
              }
            }
          }
        }
        require("lspconfig").omnisharp.setup {
          cmd = { "omnisharp", "-lsp" },
        }
        local set = vim.keymap.set
        local options = { noremap = true }
        set({ "n" }, "<leader>cf", "<cmd>lua vim.lsp.buf.format()<CR>", options)
        set({ "n" }, "gd", "<cmd>lua require('omnisharp_extended').lsp_definition()<CR>", options)
        set({ "n" }, "gD", "<cmd>lua require('omnisharp_extended').lsp_type_definition()<CR>", options)
        set({ "n" }, "gr", "<cmd>lua require('omnisharp_extended').lsp_references()<CR>", options)
        set({ "n" }, "gi", "<cmd>lua require('omnisharp_extended').lsp_implementation()<CR>", options)
      end
    },

    -- Provides better syntax highlighting/movement
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = { "nvim-treesitter/nvim-treesitter-context" },
      config = function ()
        require("nvim-treesitter.configs").setup {
          ensure_installed = { "c", "html", "lua", "query", "markdown", "markdown_inline", "vim", "vimdoc" },
          sync_install = false,
          auto_install = false,
          highlight = {
            enable = true,
            additional_vim_regex_highlighting = false
          },
        }
      end
    },

    -- Provides navigation and search pop up windows
    {
      "nvim-telescope/telescope.nvim",
      dependencies = { "nvim-lua/plenary.nvim" },
      config = function()
        local actions = require("telescope.actions")
        require("telescope").setup({
          defaults = {
            mappings = {
              i = {
                ["<esc>"] = actions.close,
              },
            },
          },
        })
      end
    },

    -- Markdown
    { "MeanderingProgrammer/render-markdown.nvim" },

    -- Auto indent on new line
    { "lukas-reineke/indent-blankline.nvim" },
  },
  checker = { enabled = true }
})

require("user.netrw")
