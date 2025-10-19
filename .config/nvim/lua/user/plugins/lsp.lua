-- LSP configurations
return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/snacks.nvim",
    },
    ft = { "lua", "c", "cs", "python", "typescript", "javascript", "vue", "vb" },
    config = function()
      vim.lsp.config("lua_ls", {
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
      })
      vim.lsp.enable("lua_ls")
      vim.lsp.config("clangd", {})
      vim.lsp.enable("clangd")
      vim.lsp.config("omnisharp", {
        cmd = { "omnisharp", "-lsp" },
        filetypes = { "cs", "vb" }
      })
      vim.lsp.enable("omnisharp")
      vim.lsp.config("jedi-language-server", {
        cmd = { "jedi-language-server" },
        filetypes = { "python" },
        root_markers = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", "Pipfile", ".git" }
      })
      vim.lsp.enable("jedi-language-server")
      vim.lsp.config("ts_ls", {
        init_options = {
          plugins = {
            {
              name = "@vue/typescript-plugin",
              location = require("os").getenv("HOME") .. "/.local/lib/node_modules/@vue/typescript-plugin",
              languages = { "typescript", "javascript", "vue" }
            }
          }
        },
        filetypes = { "typescript", "javascript", "vue" },
      })
      vim.lsp.enable("ts_ls")
      vim.lsp.enable("vue_ls")
    end,
    keys = {
      { "gd", function() Snacks.picker.lsp_definitions() end, desc = "Goto Definition" },
      { "gD", function() Snacks.picker.lsp_declarations() end, desc = "Goto Declaration" },
      { "gr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
      { "gI", function() Snacks.picker.lsp_implementations() end, desc = "Goto Implementation" },
      { "gy", function() Snacks.picker.lsp_type_definitions() end, desc = "Goto T[y]pe Definition" },
      { "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code actions" },
      { "<leader>cf", "<cmd>lua vim.lsp.buf.format()<cr>",      desc = "Format document" },
      { "<leader>ci", "<cmd>lua vim.lsp.buf.hover()<cr>",       desc = "Information" },
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
  },
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  }
}
