{ lib, ... }:

{
  programs.vscode = {
    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;

      userSettings = {
      # UI & Theme Settings
      "workbench.iconTheme" = "vscode-icons";
      "workbench.colorTheme" = "Pop Cosmic";
      "window.zoomLevel" = 1;
      "editor.fontFamily" = "'FiraCode Nerd Font', monospace";
      "editor.fontLigatures" = true;
      "editor.fontWeight" = "400";
      "editor.lineNumbers" = "relative";
      "editor.wordWrap" = "on";
      "editor.inlineSuggest.enabled" = true;
      "editor.unicodeHighlight.ambiguousCharacters" = false;
      "editor.stickyScroll.enabled" = false;

      # Terminal Settings
      "terminal.integrated.defaultProfile.linux" = "fish";
      "terminal.external.linuxExec" = "gnome-terminal";

      # Git Settings
      "git.autofetch" = true;
      "git.openRepositoryInParentFolders" = "always";

      # NPM Settings
      "npm.packageManager" = "pnpm";
      "npm.enableRunFromFolder" = true;

      # PHP Artisan Settings
      "artisan.docker.enabled" = true;
      "artisan.docker.command" = "docker compose run --rm php";

      # Go Language Settings
      "go.toolsManagement.autoUpdate" = true;
      "editor.inlayHints.enabled" = "on";
      "go.inlayHints.compositeLiteralFields" = true;
      "go.inlayHints.parameterNames" = true;
      "go.inlayHints.functionTypeParameters" = true;
      "go.inlayHints.assignVariableTypes" = true;
      "go.inlayHints.compositeLiteralTypes" = true;
      "go.inlayHints.constantValues" = true;
      "go.inlayHints.rangeVariableTypes" = true;

      # Svelte Settings
      "svelte.enable-ts-plugin" = true;
      "svelte.plugin.svelte.defaultScriptLanguage" = "ts";

      # Elixir Settings
      "elixirLS.fetchDeps" = true;
      "elixirLS.autoBuild" = true;
      "elixirLS.dialyzerEnabled" = true;
      "elixirLS.dialyzerFormat" = "dialyzer";
      "elixirLS.suggestSpecs" = true;

      # Playwright Settings
      "playwright.reuseBrowser" = false;
      "playwright.showTrace" = false;

      # LOVE2D Settings
      "pixelbyte.love2d.path" = "/usr/bin/love";
      "pixelbyte.love2d.revealStatus" = true;
      "pixelbyte.love2d.runOnSave" = true;

      # Prisma Settings
      "prisma.showPrismaDataPlatformNotification" = false;

      # C++ Language Settings
      "C_Cpp.inlayHints.autoDeclarationTypes.enabled" = true;
      "[cpp]" = {
        "editor.formatOnSave" = true;
      };

      # Python Settings
      "python.analysis.inlayHints.functionReturnTypes" = true;
      "python.analysis.inlayHints.pytestParameters" = true;
      "python.analysis.inlayHints.variableTypes" = true;
      "python.analysis.autoImportCompletions" = true;
      "python.analysis.autoFormatStrings" = true;
      "python.analysis.typeCheckingMode" = "strict";

      # Vue.js Settings
      "[vue]" = {
        "editor.defaultFormatter" = "Vue.volar";
      };

      # Tailwind/CVA Settings
      "tailwindCSS.experimental.classRegex" = [
        [ "cva\\(([^)]*)\\)" "[\"'`]([^\"'`]*).*?[\"'`]" ]
      ];

      # Lua Settings
      "[lua]" = {
        "editor.defaultFormatter" = "sumneko.lua";
      };
      };
    };
  };
}
