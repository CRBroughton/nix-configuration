# Firefox Sync Server

Self-hosted Firefox sync storage for bookmarks, open tabs, history, passwords, and preferences.

> **Note:** This handles *storage* only. Account auth still goes through Mozilla's servers (`accounts.firefox.com`).

## Setup

### 1. Configure `.env`

```sh
cp .env.example .env
```

Fill in:

- `TS_AUTHKEY` — Tailscale auth key from https://login.tailscale.com/admin/settings/keys
- `SYNCSERVER_SECRET` — generate with:
  ```sh
  openssl rand -hex 32
  ```

### 2. Start the service

```sh
podman compose up -d
```

### 3. Point Firefox at this server

In Firefox, go to `about:config` and set:

| Preference | Value |
|---|---|
| `identity.sync.tokenserver.uri` | `https://syncserver.tail538465.ts.net/token/1.0/sync/1.5` |

### 4. Create a Mozilla account and sign in

Create account at https://accounts.firefox.com if you don't have one.

Then: **Settings → Sync → Sign In**

Firefox will now store all sync data on this server.

### 5. Lock registrations (after first account)

Once signed in, set in `.env`:

```
SYNCSERVER_ALLOW_NEW_USERS=false
```

Then restart:

```sh
podman compose up -d
```
