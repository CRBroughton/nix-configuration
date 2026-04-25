# Mum's Zen browser config
{ pkgs, firefox-addons, ... }:

{
  programs.zen-flatpak = {
    enable = true;
    profile = "Default (release)";

    extensions = with firefox-addons.packages.${pkgs.stdenv.hostPlatform.system}; [
      ublock-origin
      bitwarden
    ];

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

    settings = {
      "browser.search.suggest.enabled" = false;
      "browser.urlbar.suggest.searches" = false;
      "privacy.donottrackheader.enabled" = true;
    };
  };
}
