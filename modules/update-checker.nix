{ pkgs, lib, ... }:

{
  # Creates an activation script to check for updates
  # Usage: mkUpdateChecker { name, currentVersion, fetchLatest, updateInstructions, availableVersions ? [] }
  mkUpdateChecker =
    {
      name,
      currentVersion,
      fetchLatest,
      updateInstructions,
      availableVersions ? [ ],
    }:
    let
      versionList = lib.concatStringsSep " " availableVersions;
      checkScript =
        if availableVersions == [ ] then
          # If no available versions list provided, check against current version
          ''
            if [ -n "$LATEST_VERSION" ] && [ "$LATEST_VERSION" != "${currentVersion}" ]; then
              SHOULD_NOTIFY=true
            fi
          ''
        else
          # Check if latest version is not in the available versions list
          ''
            if [ -n "$LATEST_VERSION" ]; then
              VERSION_AVAILABLE=false
              for v in ${versionList}; do
                if [ "$LATEST_VERSION" = "$v" ]; then
                  VERSION_AVAILABLE=true
                  break
                fi
              done
              if [ "$VERSION_AVAILABLE" = "false" ]; then
                SHOULD_NOTIFY=true
              fi
            fi
          '';
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      echo "Checking for ${name} updates..."
      LATEST_VERSION=$( (${fetchLatest}) || echo "")
      SHOULD_NOTIFY=false
      ${checkScript}
      if [ "$SHOULD_NOTIFY" = "true" ]; then
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
