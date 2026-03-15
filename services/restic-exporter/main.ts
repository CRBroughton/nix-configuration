import {
  createApp,
  createRouter,
  defineEventHandler,
  toNodeListener,
} from "h3";
import { createServer } from "node:http";
import { spawn } from "node:child_process";

// Configuration via environment variables
const RESTIC_REPOSITORY = process.env.RESTIC_REPOSITORY;
const REFRESH_INTERVAL = parseInt(process.env.REFRESH_INTERVAL || "3600", 10) * 1000;
const PORT = parseInt(process.env.PORT || "8080", 10);

interface Snapshot {
  time: string;
  hostname: string;
  paths: string[];
  id: string;
}

interface Stats {
  status: "initializing" | "ok" | "error";
  last_updated: string | null;
  error: string | null;
  snapshot_count: number;
  latest_snapshot: Snapshot | null;
  total_size: string | null;
  total_size_bytes: number;
  restore_size: string | null;
  restore_size_bytes: number;
  dedupe_ratio: number | null;
  compression_ratio: number | null;
}

let cachedStats: Stats = {
  status: "initializing",
  last_updated: null,
  error: null,
  snapshot_count: 0,
  latest_snapshot: null,
  total_size: null,
  total_size_bytes: 0,
  restore_size: null,
  restore_size_bytes: 0,
  dedupe_ratio: null,
  compression_ratio: null,
};

function runResticCommand(args: string[]): Promise<{ success: boolean; output: string }> {
  return new Promise((resolve) => {
    const proc = spawn("restic", ["--json", ...args], {
      env: process.env,
      timeout: 300_000, // 5 minutes
    });

    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      if (code === 0) {
        resolve({ success: true, output: stdout });
      } else {
        resolve({ success: false, output: stderr || `Exit code: ${code}` });
      }
    });

    proc.on("error", (err) => {
      resolve({ success: false, output: err.message });
    });
  });
}

function formatBytes(bytes: number): string {
  const units = ["B", "KiB", "MiB", "GiB", "TiB", "PiB"];
  let value = bytes;
  let unitIndex = 0;

  while (Math.abs(value) >= 1024 && unitIndex < units.length - 1) {
    value /= 1024;
    unitIndex++;
  }

  return `${value.toFixed(2)} ${units[unitIndex]}`;
}

async function fetchStats(): Promise<void> {
  // Get snapshots
  const snapshotsResult = await runResticCommand(["snapshots"]);
  if (!snapshotsResult.success) {
    cachedStats = {
      ...cachedStats,
      status: "error",
      error: snapshotsResult.output,
      last_updated: new Date().toISOString(),
    };
    return;
  }

  let snapshots: any[];
  try {
    snapshots = snapshotsResult.output.trim()
      ? JSON.parse(snapshotsResult.output)
      : [];
  } catch (e) {
    cachedStats = {
      ...cachedStats,
      status: "error",
      error: `Failed to parse snapshots: ${e}`,
      last_updated: new Date().toISOString(),
    };
    return;
  }

  // Get raw-data stats
  const rawStatsResult = await runResticCommand(["stats", "--mode", "raw-data"]);
  if (!rawStatsResult.success) {
    cachedStats = {
      ...cachedStats,
      status: "error",
      error: rawStatsResult.output,
      last_updated: new Date().toISOString(),
    };
    return;
  }

  let rawStats: any;
  try {
    rawStats = rawStatsResult.output.trim()
      ? JSON.parse(rawStatsResult.output)
      : {};
  } catch (e) {
    cachedStats = {
      ...cachedStats,
      status: "error",
      error: `Failed to parse stats: ${e}`,
      last_updated: new Date().toISOString(),
    };
    return;
  }

  // Get restore-size stats for dedupe ratio
  const restoreStatsResult = await runResticCommand(["stats", "--mode", "restore-size"]);
  let restoreSizeBytes = 0;
  if (restoreStatsResult.success) {
    try {
      const restoreStats = restoreStatsResult.output.trim()
        ? JSON.parse(restoreStatsResult.output)
        : {};
      restoreSizeBytes = restoreStats.total_size || 0;
    } catch {
      // Ignore parse errors for restore stats
    }
  }

  // Process latest snapshot
  let latestSnapshot: Snapshot | null = null;
  if (snapshots.length > 0) {
    const sorted = snapshots.sort(
      (a, b) => new Date(b.time).getTime() - new Date(a.time).getTime()
    );
    const latest = sorted[0];
    latestSnapshot = {
      time: latest.time,
      hostname: latest.hostname,
      paths: latest.paths || [],
      id: latest.short_id || latest.id?.slice(0, 8) || "",
    };
  }

  // Calculate dedupe ratio
  const rawSizeBytes = rawStats.total_size || 0;
  const dedupeRatio =
    rawSizeBytes > 0 && restoreSizeBytes > 0
      ? Math.round((restoreSizeBytes / rawSizeBytes) * 100) / 100
      : null;

  cachedStats = {
    status: "ok",
    last_updated: new Date().toISOString(),
    error: null,
    snapshot_count: snapshots.length,
    latest_snapshot: latestSnapshot,
    total_size: formatBytes(rawSizeBytes),
    total_size_bytes: rawSizeBytes,
    restore_size: formatBytes(restoreSizeBytes),
    restore_size_bytes: restoreSizeBytes,
    dedupe_ratio: dedupeRatio,
    compression_ratio: rawStats.compression_ratio || null,
  };
}

async function startStatsUpdater(): Promise<void> {
  while (true) {
    try {
      console.log("Fetching restic stats...");
      await fetchStats();
      console.log(`Stats updated: ${cachedStats.status}`);
    } catch (e) {
      cachedStats = {
        ...cachedStats,
        status: "error",
        error: String(e),
        last_updated: new Date().toISOString(),
      };
    }
    await new Promise((resolve) => setTimeout(resolve, REFRESH_INTERVAL));
  }
}

// Set up h3 app
const app = createApp();
const router = createRouter();

router.get(
  "/health",
  defineEventHandler(() => ({
    status: cachedStats.status === "ok" ? "healthy" : "unhealthy",
  }))
);

router.get(
  "/",
  defineEventHandler(() => cachedStats)
);

router.get(
  "/stats",
  defineEventHandler(() => cachedStats)
);

app.use(router);

// Start server
function main(): void {
  if (!RESTIC_REPOSITORY) {
    console.error("ERROR: RESTIC_REPOSITORY environment variable is required");
    process.exit(1);
  }

  console.log("Restic Exporter starting...");
  console.log(`Repository: ${RESTIC_REPOSITORY}`);
  console.log(`Refresh interval: ${REFRESH_INTERVAL / 1000}s`);
  console.log(`Port: ${PORT}`);

  // Start HTTP server first so it's responsive immediately
  const server = createServer(toNodeListener(app));
  server.listen(PORT, () => {
    console.log(`Server listening on port ${PORT}`);
    // Start background updater after server is ready
    startStatsUpdater();
  });
}

main();
