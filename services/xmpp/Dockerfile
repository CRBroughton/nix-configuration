# Prosody 0.12 with http_file_share built-in
FROM debian:bookworm-slim

# Install Prosody 0.12 from official repo
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        gnupg \
        curl \
        ca-certificates \
        lua-dbi-sqlite3 \
        lua-unbound && \
    curl -fsSL https://prosody.im/files/prosody-debian-packages.key | gpg --dearmor -o /usr/share/keyrings/prosody.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/prosody.gpg] https://packages.prosody.im/debian bookworm main" > /etc/apt/sources.list.d/prosody.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends prosody prosody-modules && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create necessary directories
RUN mkdir -p /var/run/prosody /var/lib/prosody && \
    chown -R prosody:prosody /var/run/prosody /var/lib/prosody

EXPOSE 5222 5269 5280 5281

USER prosody
CMD ["/usr/bin/prosody", "-F"]
