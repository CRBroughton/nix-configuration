{ pkgs, lib, ... }:

{
  # Creates an activation script to check for updates
  # Usage: mkUpdateChecker { name, currentVersion, fetchLatest, updateInstructions }
  mkUpdateChecker = { name, currentVersion, fetchLatest, updateInstructions }:
    lib.hm.dag.entryAfter ["writeBoundary"] ''
      echo "Checking for ${name} updates..."
      LATEST_VERSION=$( (${fetchLatest}) || echo "")
      if [ -n "$LATEST_VERSION" ] && [ "$LATEST_VERSION" != "${currentVersion}" ]; then
        echo ""
        echo "═══════════════════════════════════════════════════════"
        echo "⚠️  ${name} update available: ${currentVersion} → $LATEST_VERSION"
        echo "═══════════════════════════════════════════════════════"
        ${updateInstructions}
        echo "═══════════════════════════════════════════════════════"
        echo ""
      fi
    '';
}
