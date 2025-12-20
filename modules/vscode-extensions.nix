{ pkgs, ... }:

{
  programs.vscode = {
    mutableExtensionsDir = true;

    profiles.default.extensions = with pkgs.vscode-marketplace; [
      anweber.vscode-httpyac
      betterthantomorrow.calva
      nimsaem.nimvscode
      # Themes
      boydmeyer.pop-dark
      antfu.theme-vitesse

      # Icons
      catppuccin.catppuccin-vsc-icons
      vscode-icons-team.vscode-icons

      # Languages - Lua
      sumneko.lua

      # Languages - PHP
      bmewburn.vscode-intelephense-client
      emeraldwalk.runonsave
      swordev.phpstan

      # Languages - TypeScript/JavaScript
      dbaeumer.vscode-eslint
      yoavbls.pretty-ts-errors
      orta.vscode-twoslash-queries
      mariusalchimavicius.json-to-ts

      # Languages - Vue
      # vue.volar - Installed manually via VSCode marketplace due to Nix read-only filesystem issue
      craigrbroughton.modern-vue-snippets

      # Languages - Go
      golang.go

      # Languages - Elixir
      jakebecker.elixir-ls

      # Languages - Zig
      ziglang.vscode-zig

      # Languages - Nix
      bbenoist.nix

      # CSS/Styling
      bradlc.vscode-tailwindcss
      antfu.unocss

      # Git
      eamodio.gitlens
      gxl.git-graph-3
      vivaxy.vscode-conventional-commits

      # Utilities
      usernamehw.errorlens
      antfu.goto-alias
      craigrbroughton.htmx-attributes
      afzalsayed96.icones
      antfu.iconify
      antfu.pnpm-catalog-lens
      oven.bun-vscode
      adpyke.codesnap
      formulahendry.auto-rename-tag

      anthropic.claude-code
      peterj.proto
    ];
  };
}
