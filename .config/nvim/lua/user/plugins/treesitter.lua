-- Treesitter: provides better syntax highlighting/movement
return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  dependencies = { "nvim-treesitter/nvim-treesitter-context" },
  config = function()
    require("nvim-treesitter.configs").setup {
      ensure_installed = { "c", "html", "lua", "query", "markdown", "markdown_inline", "vim", "vimdoc" },
      sync_install = false,
      auto_install = false,
      fold = {
        enable = true
      },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false
      },
      incremental_selection = {
        enable = true
      },
      indent = {
        enable = true,
      },
      textobjects = {
        enable = true
      }
    }
  end
}
