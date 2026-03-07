# Craig's personal flatpak apps
{ config, pkgs, ... }:

{
  services.flatpak.packages = [
    "com.brave.Browser"
    "md.obsidian.Obsidian"
    "com.heroicgameslauncher.hgl"
    "io.github.equicord.equibop"
    "com.transmissionbt.Transmission"
    "io.github.flattool.Warehouse"
    "it.mijorus.gearlever"
    "net.runelite.RuneLite"
    "io.gitlab.news_flash.NewsFlash"
    "com.github.neithern.g4music"
  ];
}
