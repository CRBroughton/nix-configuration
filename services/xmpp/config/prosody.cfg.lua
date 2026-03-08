-- Prosody XMPP Server Configuration
-- Documentation: https://prosody.im/doc/configure

---------- Server-wide settings ----------

-- Allow running as root (needed for rootless Podman)
run_as_root = true

-- Admin accounts (full JID)
admins = { "CRBroughton@xmpp.tail538465.ts.net" }

-- Network interfaces to listen on
interfaces = { "*" }

-- Modules to load
modules_enabled = {
    -- Generally required
    "disco";           -- Service discovery
    "roster";          -- User contact lists
    "saslauth";        -- Authentication
    "tls";             -- TLS encryption

    -- Nice to have
    "blocklist";       -- Block users
    "carbons";         -- Message synchronization
    "csi";             -- Client state indication
    "mam";             -- Message Archive Management (history)
    "pep";             -- Personal Eventing Protocol
    "private";         -- Private XML storage
    "vcard4";          -- User profiles
    "vcard_legacy";    -- Compatibility with legacy vCards

    -- Connection reliability
    "smacks";          -- Stream Management (XEP-0198) - resume after disconnect
    "bookmarks";       -- Bookmark sync across clients (XEP-0402)

    -- Voice/video calls
    "turn_external";   -- Advertise STUN/TURN servers for Jingle calls

    -- Admin tools
    "admin_adhoc";     -- Admin commands
    "admin_shell";     -- prosodyctl shell commands
    "register";        -- Account management (needed for password changes)
    "announce";        -- Broadcast messages to users (admin only)
    -- "motd";            -- Message of the day on login
    "watchregistrations";  -- Notify admins of new signups via XMPP

    -- HTTP modules (for web clients)
    "bosh";            -- BOSH support
    "websocket";       -- WebSocket support
}

modules_disabled = {
    "s2s";  -- Disable server-to-server (private server)
}

-- Disable telemetry
statistics = "internal"

-- Authentication
authentication = "internal_hashed"

-- Storage
storage = "internal"

-- Archive settings (message history)
archive_expires_after = "10y"  -- Keep messages for 1 year
default_archive_policy = true  -- Archive by default

-- Logging
log = {
    info = "*console";
}

-- TLS certificates
certificates = "/etc/prosody/certs"

ssl = {
    certificate = "/etc/prosody/certs/xmpp.tail538465.ts.net.crt";
    key = "/etc/prosody/certs/xmpp.tail538465.ts.net.key";
}

-- Security settings
c2s_require_encryption = true

-- STUN/TURN for voice/video calls (XEP-0215)
-- Using public STUN server (no auth needed for STUN)
-- Since all clients are on Tailscale, they connect directly after STUN discovery
turn_external_host = "stun.l.google.com"
turn_external_port = 19302
turn_external_secret = "unused"  -- Not used for STUN-only

-- HTTP settings (for BOSH/WebSocket)
-- Tailscale serve handles HTTPS termination on port 443
http_ports = { 5280 }
http_interfaces = { "*" }
https_ports = {}
https_interfaces = {}

-- cross_domain options deprecated in Prosody 0.12+

-- Message of the Day (shown once per login)
motd_text = [[Welcome to the XMPP server!]]

---------- Virtual hosts ----------

VirtualHost "xmpp.tail538465.ts.net"
    allow_registration = false

---------- Components ----------

-- Multi-User Chat (rooms)
Component "rooms.xmpp.tail538465.ts.net" "muc"
    modules_enabled = { "muc_mam" }  -- Message history in rooms
    restrict_room_creation = true    -- Only admins can create rooms

-- HTTP File Upload (Prosody 0.12 built-in)
Component "upload.xmpp.tail538465.ts.net" "http_file_share"
    http_file_share_size_limit = 104857600  -- 100MB max file size
    http_file_share_expires_after = 604800  -- Files expire after 7 days
    http_host = "xmpp.tail538465.ts.net"
    http_external_url = "https://xmpp.tail538465.ts.net"
