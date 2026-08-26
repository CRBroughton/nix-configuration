package main

import (
	"crypto/subtle"
	"encoding/json"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"os/exec"
	"strings"
)

var (
	port         = getEnv("PORT", "8088")
	unit         = getEnv("UNIT", "terraria.service")
	lanInterface = getEnv("LAN_INTERFACE", "eno1")
	gamePort     = getEnv("GAME_PORT", "7777")
	// Fixed paths, not resolved via $PATH: systemd gives services a minimal
	// PATH, and the sudoers rule in terraria-control.nix matches against
	// this exact systemctl path — resolving it differently here would
	// silently fail to match and sudo would refuse.
	sudoBin      = getEnv("SUDO_BIN", "/run/wrappers/bin/sudo")
	systemctlBin = getEnv("SYSTEMCTL_BIN", "/run/current-system/sw/bin/systemctl")
	token        = readToken(os.Getenv("TOKEN_FILE"))
)

func getEnv(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

func readToken(path string) string {
	data, err := os.ReadFile(path)
	if err != nil {
		log.Fatalf("failed to read TOKEN_FILE: %v", err)
	}
	return strings.TrimSpace(string(data))
}

func lanIP() string {
	iface, err := net.InterfaceByName(lanInterface)
	if err != nil {
		return ""
	}
	addrs, err := iface.Addrs()
	if err != nil {
		return ""
	}
	for _, a := range addrs {
		if ipNet, ok := a.(*net.IPNet); ok {
			if ip4 := ipNet.IP.To4(); ip4 != nil {
				return ip4.String()
			}
		}
	}
	return ""
}

func isActive() string {
	out, _ := exec.Command(systemctlBin, "is-active", unit).Output()
	return strings.TrimSpace(string(out))
}

func authorized(r *http.Request) bool {
	expected := "Bearer " + token
	got := r.Header.Get("Authorization")
	return subtle.ConstantTimeCompare([]byte(got), []byte(expected)) == 1
}

func handleStatus(w http.ResponseWriter, r *http.Request) {
	state := isActive()
	json.NewEncoder(w).Encode(map[string]any{
		"active": state == "active",
		"state":  state,
		"ip":     lanIP(),
		"port":   gamePort,
	})
}

func handleAction(action string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			w.WriteHeader(http.StatusMethodNotAllowed)
			return
		}
		if !authorized(r) {
			w.WriteHeader(http.StatusUnauthorized)
			fmt.Fprint(w, "unauthorized")
			return
		}
		out, err := exec.Command(sudoBin, systemctlBin, action, unit).CombinedOutput()
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Fprintf(w, "%s\n%s", err, out)
			return
		}
		fmt.Fprintf(w, "%s ok", action)
	}
}

const pageTemplate = `<!doctype html>
<html><head><meta charset="utf-8"><title>Terraria Control</title>
<style>
  body { font-family: system-ui, sans-serif; background: #1a1a1a; color: #eee; padding: 2rem; }
  .status { font-size: 1.2rem; margin-bottom: 1rem; }
  .active { color: #4caf50; }
  .inactive { color: #f44336; }
  button { font-size: 1rem; padding: 0.6rem 1.4rem; border: none; border-radius: 6px; cursor: pointer; margin-right: 0.5rem; }
  .start { background: #4caf50; color: white; }
  .stop { background: #f44336; color: white; }
  code { background: #333; padding: 0.2rem 0.5rem; border-radius: 4px; }
</style></head>
<body>
  <div class="status">Status: <span class="%[1]s">%[2]s</span></div>
  <div class="status">Connect: <code>%[3]s:%[4]s</code></div>
  <button class="start" onclick="call('start')">Start</button>
  <button class="stop" onclick="call('stop')">Stop</button>
  <pre id="log"></pre>
  <script>
    const TOKEN = %[5]q;
    async function call(action) {
      const res = await fetch('/' + action, { method: 'POST', headers: { Authorization: 'Bearer ' + TOKEN } });
      document.getElementById('log').textContent = await res.text();
      setTimeout(() => location.reload(), 1500);
    }
  </script>
</body></html>`

func handleIndex(w http.ResponseWriter, r *http.Request) {
	state := isActive()
	cls := "inactive"
	if state == "active" {
		cls = "active"
	}
	fmt.Fprintf(w, pageTemplate, cls, state, lanIP(), gamePort, token)
}

func main() {
	http.HandleFunc("/", handleIndex)
	http.HandleFunc("/status", handleStatus)
	http.HandleFunc("/start", handleAction("start"))
	http.HandleFunc("/stop", handleAction("stop"))

	// Loopback-only: exposed to the tailnet exclusively via `tailscale serve`
	// (see terraria-control.nix), which terminates HTTPS. Not bound to
	// tailscale0 directly so there's no plain-http path onto the tailnet.
	addr := "127.0.0.1:" + port
	log.Printf("terraria-control listening on %s", addr)
	log.Fatal(http.ListenAndServe(addr, nil))
}
