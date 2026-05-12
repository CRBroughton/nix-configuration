#!/usr/bin/env bash
mkdir -p ~/.local/share/gnome-remote-desktop
openssl req -x509 -nodes -newkey rsa:2048 -keyout ~/.local/share/gnome-remote-desktop/rdp-tls.key -out ~/.local/share/gnome-remote-desktop/rdp-tls.crt -days 3650 -subj "/CN=mum-pc"
grdctl rdp set-tls-cert ~/.local/share/gnome-remote-desktop/rdp-tls.crt
grdctl rdp set-tls-key ~/.local/share/gnome-remote-desktop/rdp-tls.key
grdctl rdp enable
