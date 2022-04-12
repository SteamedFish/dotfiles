local M = {}
M.source = debug.getinfo(1, "S").source:sub(2)

M.modules = {
  ui = {
    "dashboard",          -- Start screen
    -- "doom-themes",     -- Additional doom emacs' colorschemes
    "indentlines",        -- Show indent lines
    -- "show_registers",  -- popup that shows register contents
    "statusline",         -- Statusline
    "tabline",            -- Tabline, shows your buffers list at top
    "which-key",          -- Keybindings popup menu like Emacs' guide-key
    -- "zen",             -- Distraction free environment
  },
  doom = {
    -- "compiler",        -- Compile (and run) your code with just pressing three keys!
    -- "neorg",           -- Life Organization Tool
    -- "runner",          -- Open a REPL for the current language or run the current file
  },
  editor = {
    "autopairs",          -- Autopairs
    "auto-session",       -- A small automated session manager for Neovim
    "dap",                -- Debug Adapter Protocol
    "editorconfig",       -- EditorConfig support for Neovim
    "explorer",           -- Tree explorer
    "formatter",          -- File formatting
    "gitsigns",           -- Git signs
    "kommentary",         -- Comments plugin
    "lsp",                -- Language Server Protocols
    -- "minimap",         -- Code minimap, requires github.com/wfxr/code-minimap
    -- "ranger",          -- Ranger File Browser, requires ranger file browser
    "snippets",           -- LSP snippets
    "symbols",            -- LSP symbols and tags
    "telescope",          -- Highly extendable fuzzy finder over lists
    -- "terminal",        -- Terminal for Neovim (NOTE: needed for runner and compiler)
  },
  langs = {
    -- To enable the language server for a language just add the +lsp flag
    -- at the end, e.g. 'rust +lsp'. This will install the rust TreeSitter
    -- parser and rust-analyzer
    --
    "bash",               -- The terminal gods language
    "config",             -- Configuration files (JSON, YAML, TOML)
    "cpp",                -- C++ support
    "css",                -- CSS support
    "dockerfile",         -- Do you like containers, right?
    -- "elixir",          -- Build scalable and maintainable software
    "go +lsp",            -- Hello, gopher
    -- "haskell",         -- Because Functional programming is fun, isn't it?
    "html",               -- HTML support
    -- "java",            -- Java support
    "javascript",         -- JavaScript support
    "lua",                -- Support for our gods language
    "python +lsp",        -- Python support + lsp
    -- "ruby",            -- Look ma, I love the gems!
    "rust +lsp",          -- Let's get rusty!
    -- "typescript",      -- TypeScript support
  },
  utilities = {
    -- "lazygit",         -- LazyGit integration for Neovim, requires LazyGit
    -- "neogit",          -- Magit for Neovim
    "range-highlight",    -- hightlights ranges you have entered in commandline
    -- "suda",            -- Write and read files without sudo permissions
  },
  web = {
    "colorizer",          -- Fastest colorizer for Neovim
    -- "firenvim",        -- requires firenvim browser extension; change fontsize by increasing guifontsize in doom_config
    -- "restclient",      -- A fast Neovim http client
  }
}

return M
