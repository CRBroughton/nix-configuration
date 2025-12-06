{ pkgs, ... }:

{
  home.packages = [
    (pkgs.writeShellScriptBin "laravel-api-cleanup" ''
      #!/usr/bin/env bash

      # Laravel API-Only Cleanup Script
      # Removes frontend files and dependencies from existing Laravel projects
      # Usage: laravel-api-cleanup [project-path]
      # If no path is provided, runs in current directory

      set -e  # Exit on error

      # Determine target directory
      if [ -z "$1" ]; then
          TARGET_DIR="."
      else
          TARGET_DIR="$1"
      fi

      # Check if directory exists
      if [ ! -d "$TARGET_DIR" ]; then
          echo "Error: Directory '$TARGET_DIR' does not exist"
          exit 1
      fi

      # Check if it's a Laravel project
      if [ ! -f "$TARGET_DIR/artisan" ]; then
          echo "Error: '$TARGET_DIR' does not appear to be a Laravel project (artisan file not found)"
          exit 1
      fi

      cd "$TARGET_DIR"
      PROJECT_NAME=$(basename "$(pwd)")

      echo "Cleaning up frontend files from: $PROJECT_NAME"
      echo "Working directory: $(pwd)"
      echo ""

      echo "Removing frontend files and dependencies..."

      # Remove frontend configuration files
      rm -f vite.config.js
      rm -f postcss.config.js
      rm -f tailwind.config.js
      rm -f package.json
      rm -f package-lock.json
      rm -f pnpm-lock.yaml
      rm -f yarn.lock

      # Remove frontend directories
      rm -rf resources/js
      rm -rf resources/css
      rm -rf node_modules

      # Remove welcome view (frontend route)
      rm -f resources/views/welcome.blade.php

      # Remove web routes (optional - keep if you need basic web routes)
      if [ -f "routes/web.php" ]; then
          echo "Cleaning up web routes..."
          echo "<?php

use Illuminate\Support\Facades\Route;

// Web routes removed for API-only application
// All API routes should be defined in routes/api.php
" > routes/web.php
      fi

      # Update composer.json to remove frontend-related scripts
      if [ -f "composer.json" ]; then
          echo "Updating composer.json..."
          ${pkgs.php}/bin/php -r '
$composer = json_decode(file_get_contents("composer.json"), true);

// Remove post-update-cmd scripts related to npm
if (isset($composer["scripts"]["post-update-cmd"])) {
    $composer["scripts"]["post-update-cmd"] = array_filter(
        $composer["scripts"]["post-update-cmd"],
        function($cmd) {
            return !preg_match("/npm|vite/i", $cmd);
        }
    );
}

// Remove post-install-cmd scripts related to npm
if (isset($composer["scripts"]["post-install-cmd"])) {
    $composer["scripts"]["post-install-cmd"] = array_filter(
        $composer["scripts"]["post-install-cmd"],
        function($cmd) {
            return !preg_match("/npm|vite/i", $cmd);
        }
    );
}

file_put_contents("composer.json", json_encode($composer, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES) . "\n");
'
      fi

      echo ""
      echo "✓ Frontend cleanup completed for '$PROJECT_NAME'!"
      echo ""
    '')

    (pkgs.writeShellScriptBin "laravel-modules-cleanup" ''
      #!/usr/bin/env bash

      # Laravel Modules API-Only Cleanup Script
      # Runs laravel-api-cleanup on all subdirectories in a modules folder
      # Usage: laravel-modules-cleanup [modules-path]
      # If no path is provided, looks for 'modules' or 'Modules' in current directory

      set -e  # Exit on error

      # Determine modules directory
      if [ -z "$1" ]; then
          if [ -d "modules" ]; then
              MODULES_DIR="modules"
          elif [ -d "Modules" ]; then
              MODULES_DIR="Modules"
          else
              echo "Error: No modules directory found in current directory"
              echo "Usage: laravel-modules-cleanup [modules-path]"
              exit 1
          fi
      else
          MODULES_DIR="$1"
      fi

      # Check if directory exists
      if [ ! -d "$MODULES_DIR" ]; then
          echo "Error: Directory '$MODULES_DIR' does not exist"
          exit 1
      fi

      echo "Processing Laravel modules in: $MODULES_DIR"
      echo ""

      # Counter for processed modules
      PROCESSED=0
      SKIPPED=0

      # Iterate through subdirectories
      for MODULE_PATH in "$MODULES_DIR"/*/ ; do
          if [ -d "$MODULE_PATH" ]; then
              MODULE_NAME=$(basename "$MODULE_PATH")

              # Check if it's a Laravel module (has artisan file)
              if [ -f "$MODULE_PATH/artisan" ]; then
                  echo "======================================"
                  echo "Processing module: $MODULE_NAME"
                  echo "======================================"
                  laravel-api-cleanup "$MODULE_PATH"
                  ((PROCESSED++))
                  echo ""
              else
                  echo "Skipping '$MODULE_NAME' (not a Laravel project)"
                  ((SKIPPED++))
              fi
          fi
      done

      echo "======================================"
      echo "Summary:"
      echo "  Processed: $PROCESSED modules"
      echo "  Skipped: $SKIPPED directories"
      echo "======================================"
    '')
  ];
}
