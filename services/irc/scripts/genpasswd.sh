#!/bin/bash
# Generate a bcrypt password hash for Ergo operator accounts
# Usage: ./genpasswd.sh

echo "Enter the password to hash:"
podman exec -it ergo ergo genpasswd
