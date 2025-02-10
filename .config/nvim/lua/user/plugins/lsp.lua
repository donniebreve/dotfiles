-- LSP configurations
return {
  {
    "neovim/nvim-lspconfig",
    lazy = true,
    ft = { "lua", "cs", "vb" },
    config = function()
      require("lspconfig").lua_ls.setup {
        settings = {
          Lua = {
            -- Tell lua lsp to ignore vim global
            diagnostics = {
              globals = { "vim" }
            },
            format = {
              defaultConfig = {
                max_line_length = "999"
              }
            }
          }
        }
      }
      require("lspconfig").ts_ls.setup {}
      require("lspconfig").omnisharp.setup {
        cmd = { "omnisharp", "-lsp" },
        filetypes = { "cs", "vb" }
      }
    end,
    keys = {
      { "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code actions" },
      { "<leader>cf", "<cmd>lua vim.lsp.buf.format()<cr>",      desc = "Format document" },
      { "<leader>ci", "<cmd>lua vim.lsp.buf.hover()<cr>",       desc = "Information" }
    }
  },
  {
    "hoffs/omnisharp-extended-lsp.nvim",
    lazy = true,
    ft = { "cs", "vb" },
    keys = {
      { "gd", "<cmd>lua require('omnisharp_extended').lsp_definition()<cr>",      desc = "Go to definition",      ft = { "cs", "vb" } },
      { "gD", "<cmd>lua require('omnisharp_extended').lsp_type_definition()<cr>", desc = "Go to type definition", ft = { "cs", "vb" } },
      { "gr", "<cmd>lua require('omnisharp_extended').lsp_references()<cr>",      desc = "Go to references",      ft = { "cs", "vb" } },
      { "gi", "<cmd>lua require('omnisharp_extended').lsp_implementation()<cr>",  desc = "Go to implementation",  ft = { "cs", "vb" } },
    }
  }
}
