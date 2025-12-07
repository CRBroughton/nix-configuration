{ pkgs, firefox-addons, lib, config, ... }:

{

  programs.zen-browser = {
    enable = true;

    profiles."default" = {
        extensions.packages = with firefox-addons.packages.${pkgs.stdenv.hostPlatform.system}; [
          ublock-origin
          bitwarden
        ];

        pinsForce = true;
        pins = {
          "GitHub" = {
            id = "a1b2c3d4-e5f6-4a5b-8c9d-0e1f2a3b4c5d";
            url = "https://github.com/crbroughton";
            isEssential = true;
            position = 100;
          };
          "Monkeytype" = {
            id = "b2c3d4e5-f6a7-5b6c-9d0e-1f2a3b4c5d6e";
            url = "https://monkeytype.com";
            isEssential = true;
            position = 101;
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
      };
    };
  };
}
