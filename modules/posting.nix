{ config, pkgs, ... }:

{
  home.file = {
    # Posting configuration
    ".config/posting/config.yaml".text = ''
      theme: Nuxt
      theme_directory: ${config.home.homeDirectory}/.local/share/posting/themes
      load_user_themes: true
      load_builtin_themes: true

      # Environment
      watch_env_files: true
      watch_themes: true
      watch_collection_files: true

      # Response settings
      response:
        prettify_json: true
        show_size_and_time: true

      # URL bar
      url_bar:
        show_value_preview: true
        hide_secrets_in_value_preview: true
    '';

    # Nuxt theme
    ".local/share/posting/themes/nuxt.posting-theme.yaml".text = ''
      name: Nuxt
      primary: '#00dc82'
      secondary: '#00bd6f'
      accent: '#80eec0'
      background: '#020420'
      surface: '#0a0f2c'
      error: '#f43f5e'
      success: '#00dc82'
      warning: '#fbbf24'

      author: Craig Broughton
      description: A dark theme inspired by Nuxt.js with its signature green colors
      homepage: https://github.com/craigrbroughton/nix-configuration

      text_area:
        cursor: 'reverse'
        cursor_line: 'underline'
        selection: 'reverse #00dc82'
        gutter: 'bold #80eec0'
        matched_bracket: 'black on #00dc82'

      url:
        base: 'italic #80eec0'
        protocol: 'bold #00dc82'

      syntax:
        json_key: 'italic #00dc82'
        json_number: '#80eec0'
        json_string: '#a5f3d0'
        json_boolean: '#00bd6f'
        json_null: 'dim #80eec0'

      method:
        get: 'bold #00dc82'
        post: 'bold #00bd6f'
        put: 'bold #80eec0'
        patch: 'bold #a5f3d0'
        delete: 'bold #f43f5e'
    '';
  };
}
