{ config, pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = true;

    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;

      extensions = with pkgs.vscode-marketplace; [
        # Themes
        antfu.theme-vitesse
        catppuccin.catppuccin-vsc-icons
        vscode-icons-team.vscode-icons

        # Languages
        sumneko.lua
        bmewburn.vscode-intelephense-client
        golang.go
        ziglang.vscode-zig
        bbenoist.nix
        jakebecker.elixir-ls

        # TypeScript/JavaScript
        dbaeumer.vscode-eslint
        yoavbls.pretty-ts-errors
        orta.vscode-twoslash-queries

        # CSS
        bradlc.vscode-tailwindcss
        antfu.unocss

        # Git
        eamodio.gitlens
        gxl.git-graph-3
        vivaxy.vscode-conventional-commits

        # Utilities
        usernamehw.errorlens
        anthropic.claude-code
        anweber.vscode-httpyac
        oven.bun-vscode
        peterj.proto
      ];

      userSettings = {
        # UI
        "workbench.iconTheme" = "vscode-icons";
        "workbench.colorTheme" = "Vitesse Dark";
        "window.zoomLevel" = 1;
        "editor.fontFamily" = "'FiraCode Nerd Font', monospace";
        "editor.fontLigatures" = true;
        "editor.fontWeight" = "600";
        "editor.lineNumbers" = "relative";
        "editor.wordWrap" = "on";
        "editor.inlineSuggest.enabled" = true;

        # Terminal
        "terminal.integrated.defaultProfile.linux" = "fish";

        # Git
        "git.autofetch" = true;
        "git.openRepositoryInParentFolders" = "always";

        # Go
        "go.toolsManagement.autoUpdate" = true;
        "editor.inlayHints.enabled" = "on";

        # Zig
        "zig.path" = "zig";
        "zig.zls.enabled" = "on";
        "[zig]"."editor.formatOnSave" = true;
      };
    };
  };
}
