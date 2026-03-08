#!/bin/bash
# View Ergo logs
# Usage: ./logs.sh [--follow]

if [ "$1" == "--follow" ] || [ "$1" == "-f" ]; then
    podman logs -f ergo
else
    podman logs ergo
fi
