{
  pkgs,
  inputs,
  ...
}:

{
  home = {
    username = "nikita";
    file = {
      ".emacs".source = ./emacs/.emacs;
      ".emacs.d/acme-mouse.el".source = ./emacs/acme-mouse.el;
    };
  };

  imports = [ inputs.lazyvim.homeManagerModules.default ];
  programs.lazyvim = {
    enable = true;

    extras.lang = {
      nix.enable = true;
    };

    pluginSource = "nixpkgs";

    plugins = { 
      # adding sqlite to extraPackages doesnt work, needs this
      snacks-config = ''
        return {
          "folke/snacks.nvim",
          opts = {
            image = {
              enabled = true,
            },
            statuscolumn = {
              enabled = true,
            },
            picker = {
              db = {
                sqlite3_path = "${pkgs.lib.getLib pkgs.sqlite}/lib/libsqlite3.so",
              },
            },
          },
        }
      '';

      # nil config for importing flake inputs, silences prompt.
      # autoEvalInputs might be slow, set to false if so
      nix-lsp = ''
        return {
          "neovim/nvim-lspconfig",
          opts = {
            servers = {
              nil_ls = {
                settings = {
                  ["nil"] = {
                    nix = {
                      flake = {
                        autoArchive = true,
                        autoEvalInputs = true,
                      },
                    },
                  },
                },
              },
            },
          },
        }
      '';

      # need this to prevent a race where lualine loads before troubles done
      lualine-trouble = ''
    return {
      "nvim-lualine/lualine.nvim",
      dependencies = {
        "folke/trouble.nvim",
      },
    }
  '';
    };

    config.options = ''
            -- Use Nix-provided Lua 5.1/LuaRocks, retaining rock support.
      local rocks = require("lazy.core.config").options.rocks
      rocks.enabled = true
      rocks.hererocks = false

          -- Norg is packaged separately from nvim-treesitter's supported parsers.
          vim.opt.rtp:append(
            "${pkgs.neovimUtils.grammarToPlugin pkgs.tree-sitter-grammars.tree-sitter-norg}"
          )

          -- Actually use Snacks' status column.
          vim.opt.statuscolumn = [[%!v:lua.require'snacks.statuscolumn'.get()]]
    '';

    treesitterParsers = with pkgs.vimPlugins.nvim-treesitter-parsers; [
      css
      latex
      scss
      svelte
      typst
      vue
    ];

    extraPackages = with pkgs; [
      alejandra
      ast-grep
      ghostscript
      imagemagick
      lua-language-server
      lua51Packages.luarocks
      lua5_1
      mermaid-cli
      nil
      nixd
      shfmt
      statix
      stylua
      # XXX: uncomment when upstreams fixed
      # tectonic
      tree-sitter
    ];
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
