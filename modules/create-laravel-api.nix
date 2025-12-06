{ pkgs, ... }:

{
  home.packages = [
    (pkgs.writeShellScriptBin "create-laravel-api" ''
      #!/usr/bin/env bash

      # Laravel API-Only Project Setup Script
      # Usage: create-laravel-api <project-name>

      set -e  # Exit on error

      # Check if project name is provided
      if [ -z "$1" ]; then
          echo "Error: Project name is required"
          echo "Usage: create-laravel-api <project-name>"
          exit 1
      fi

      APP_NAME="$1"

      echo "Creating Laravel project: $APP_NAME"
      ${pkgs.php84Packages.composer}/bin/composer create-project laravel/laravel "$APP_NAME"

      cd "$APP_NAME"

      echo "Installing Laravel API..."
      ${pkgs.php}/bin/php artisan install:api

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
      echo "<?php

      use Illuminate\Support\Facades\Route;

      // Web routes removed for API-only application
      // All API routes should be defined in routes/api.php
      " > routes/web.php

      # Add health check endpoint to API routes
      echo "Adding health check endpoint..."
      cat > routes/api.php << 'EOF'
      <?php

      use Illuminate\Http\Request;
      use Illuminate\Support\Facades\Route;

      Route::get('/health', function () {
          return response()->json([
              'status' => 'ok',
              'message' => 'API is running',
              'timestamp' => now()->toIso8601String(),
          ]);
      });

      Route::get('/user', function (Request $request) {
          return $request->user();
      })->middleware('auth:sanctum');
      EOF

      # Update composer.json to remove frontend-related scripts
      echo "Updating composer.json..."
      # Using PHP to manipulate composer.json
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

      echo ""
      echo "✓ Laravel API-only project '$APP_NAME' created successfully!"
      echo ""
      echo "To start the server:"
      echo "  cd $APP_NAME && cp .env.example .env && php artisan key:generate && php artisan migrate && php artisan serve"
      echo ""
      echo "Once running, test the API at:"
      echo "  http://localhost:8000/api/health"
      echo ""
      echo "API routes can be defined in: routes/api.php"
      echo "Default API prefix: /api"
    '')
  ];
}
