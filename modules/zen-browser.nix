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

    pinsForce = true;
    pins = {
      "GitHub" = {
        url = "https://github.com/crbroughton";
        isEssential = true;
      };
      "Monkeytype" = {
        url = "https://monkeytype.com";
        isEssential = true;
        };
    };

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
