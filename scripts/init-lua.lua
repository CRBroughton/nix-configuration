#!/usr/bin/env lua

-- Initialize Lua project with .luarc.json configuration

local config_path = ".luarc.json"

-- Check if file already exists
local existing = io.open(config_path, "r")
if existing then
  existing:close()
  print("Warning: " .. config_path .. " already exists")
  io.write("Overwrite? (y/N): ")
  local answer = io.read()
  if not answer or not answer:match("^[Yy]") then
    print("Cancelled.")
    os.exit(0)
  end
end

-- Write configuration
local file = io.open(config_path, "w")
if not file then
  print("Error: Could not write to " .. config_path)
  os.exit(1)
end

-- Manually format JSON for readability
file:write('{\n')
file:write('  "$schema": "https://raw.githubusercontent.com/LuaLS/vscode-lua/master/setting/schema.json",\n')
file:write('  "runtime": {\n')
file:write('    "version": "Lua 5.2",\n')
file:write('    "path": [\n')
file:write('      "?.lua",\n')
file:write('      "?/init.lua"\n')
file:write('    ]\n')
file:write('  },\n')
file:write('  "workspace": {\n')
file:write('    "library": [\n')
file:write('      "${3rd}/busted/library",\n')
file:write('      "${3rd}/luassert/library"\n')
file:write('    ]\n')
file:write('  }\n')
file:write('}\n')

file:close()

print("✓ Created " .. config_path)
print("  - Lua 5.2 runtime configured")
print("  - Busted and luassert libraries included")
print("  - LSP support enabled")
print("")
print("Reload your editor to apply changes")
