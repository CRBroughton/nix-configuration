{ pkgs, firefox-addons, lib, config, ... }:

{

  programs.zen-flatpak = {
    enable = true;
    profile = "Default (release)";

    extensions = with firefox-addons.packages.${pkgs.stdenv.hostPlatform.system}; [
      ublock-origin
      bitwarden
      web-clipper-obsidian
    ];

    pins = [
      {
        name = "GitHub";
        url = "https://github.com/crbroughton";
        isEssential = true;
      }
      {
        name = "Monkeytype";
        url = "https://monkeytype.com";
        isEssential = true;
      }
      {
        name = "SearXNG";
        url = "http://localhost:8888";
      }
      {
        name = "Frontend";
        isGroup = true;
        children = [
          {
            title = "Javascript Docs";
            url = "https://javascript.info";
          }
          {
            title = "MDN";
            url = "https://developer.mozilla.org";
          }
        ];
      }
      {
        name = "Go";
        isGroup = true;
        children = [
          {
            title = "Go By Example";
            url = "https://gobyexample.com";
          }
          {
            title = "Effective Go";
            url = "https://go.dev/doc/effective_go";
          }
          {
            title = "Awesome Go";
            url = "https://github.com/avelino/awesome-go";
          }
        ];
      }
      {
        name = "Nix";
        isGroup = true;
        children = [
          {
            title = "Nix Packages";
            url = "https://search.nixos.org/packages";
          }
          {
            title = "Nix Dev Documentation";
            url = "https://nix.dev/index.html";
          }
          {
            title = "Nix Pills";
            url = "https://nixos.org/guides/nix-pills";
          }
        ];
      }
      {
        name = "Programming";
        isGroup = true;
        children = [
          {
            title = "Devdocs";
            url = "https://devdocs.io";
          }
          {
            title = "Exercism";
            url = "https://exercism.org";
          }
          {
            title = "Raylib";
            url = "https://www.raylib.com";
          }
        ];
      }
      {
        name = "Selfhosting";
        isGroup = true;
        children = [
          {
            title = "Github - Awesome Selfhosted";
            url = "https://github.com/awesome-selfhosted/awesome-selfhosted";
          }
        ];
      }
    ];

    settings = {
      # Privacy settings
      "browser.search.suggest.enabled" = false;
      "browser.urlbar.suggest.searches" = false;
      "privacy.donottrackheader.enabled" = true;

      # Performance
      "browser.cache.disk.enable" = true;
      "browser.sessionstore.interval" = 15000;
    };

    policies = {
      ExtensionSettings = {
        "uBlock0@raymondhill.net" = {
          installation_mode = "force_installed";
          toolbar_pin = "on";
        };
        "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
          installation_mode = "force_installed";
          toolbar_pin = "on";
        };
        "clipper@obsidian.md" = {
          installation_mode = "force_installed";
          toolbar_pin = "on";
        };
      };
    };
  };
}
