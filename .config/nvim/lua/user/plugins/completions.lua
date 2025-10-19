-- Completions
return {
  "hrsh7th/nvim-cmp",
  lazy = true,
  event = "InsertEnter",
  dependencies = {
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-nvim-lsp-signature-help" },
    { "hrsh7th/cmp-nvim-lsp-document-symbol" }
  },
  config = function()
    require("cmp_nvim_lsp").default_capabilities()
    local cmp = require("cmp")
    local sources = {
      { name = "nvim_lsp",                   priority = 1000 },
      { name = "nvim_lsp_signature_help",    priority = 900 },
      { name = "nvim_lsp_document_symbol",   priority = 901 },
      { name = "lazydev.integrations.blink", priority = 700 },
      --{ name = "luasnip", priority = 750 },
      { name = "buffer",                     priority = 500 },
      { name = "path",                       priority = 250 },
    }
    local has_render_markdown, render_markdown = pcall(require, "render-markdown")
    if has_render_markdown then
      table.insert(sources, { name = 'render-markdown' })
    end
    cmp.setup({
      mapping = cmp.mapping.preset.insert({
        -- ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        -- ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true })
      }),
      sources = require("cmp").config.sources(sources)
    })
  end
}
