_:

{
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;
    # Allow these users to push unsigned paths (for remote deployment)
    trusted-users = [
      "root"
      "@wheel"
    ];

    # Binary cache - nixos-server via Tailscale (checked before upstream)
    substituters = [
      "http://100.86.95.111:5000"
      "https://cache.nixos.org"
    ];
    trusted-public-keys = [
      "nixos-server:HwKsO2QULgIbnWor1Q/tsQUSaLZGvDsnyu5MeyFUv4s="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "brighton-pc:fK9lPG6vw1PQ3CqScRCbfqSPEyKjN827noRnyl6tMJY="
    ];
    connect-timeout = 5;
  };

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
