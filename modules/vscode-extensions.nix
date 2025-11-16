{ pkgs, ... }:

{
  programs.vscode = {
    mutableExtensionsDir = false;

    profiles.default.extensions = with pkgs.vscode-marketplace; [
      # Themes
      boydmeyer.pop-dark
      antfu.theme-vitesse

      # Icons
      catppuccin.catppuccin-vsc-icons
      vscode-icons-team.vscode-icons

      # Languages - TypeScript/JavaScript
      dbaeumer.vscode-eslint
      yoavbls.pretty-ts-errors
      orta.vscode-twoslash-queries
      mariusalchimavicius.json-to-ts

      # Languages - Vue
      vue.volar
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
    ];
  };
}
