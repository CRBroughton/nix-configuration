#!/bin/bash
# Reload Ergo configuration without restart
# Usage: ./rehash.sh

podman exec ergo kill -HUP 1
echo "Configuration reloaded (SIGHUP sent)"
