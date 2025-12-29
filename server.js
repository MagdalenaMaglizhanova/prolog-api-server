// server.js
const express = require("express");
const cors = require("cors");
const { spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001;

app.use(cors());
app.use(express.json());

// ===============================
// Supabase client
// ===============================
const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

// ===============================
// Runtime directories
// ===============================
const RUNTIME_ROOT = path.join(__dirname, "runtime");
if (!fs.existsSync(RUNTIME_ROOT)) {
  fs.mkdirSync(RUNTIME_ROOT, { recursive: true });
}

// ===============================
// Start persistent Prolog process
// ===============================
const prolog = spawn("swipl", [
  "-q",
  "-s",
  path.join(__dirname, "prolog", "main.pl")
]);

console.log("🧠 Prolog engine started");

prolog.stderr.on("data", data => {
  console.error("[PROLOG ERROR]", data.toString());
});

// Буфер за stdout (много важно)
let stdoutBuffer = "";

prolog.stdout.on("data", data => {
  stdoutBuffer += data.toString();
});

// ===============================
// Helper: send command to Prolog
// ===============================
function sendToProlog(command, timeout = 2000) {
  return new Promise((resolve, reject) => {
    stdoutBuffer = "";

    prolog.stdin.write(command.trim() + ".\n");

    const start = Date.now();
    const interval = setInterval(() => {
      if (stdoutBuffer.length > 0) {
        clearInterval(interval);
        resolve(stdoutBuffer.trim());
      }
      if (Date.now() - start > timeout) {
        clearInterval(interval);
        reject(new Error("Prolog timeout"));
      }
    }, 50);
  });
}

// ===============================
// Helper: load domain from Supabase
// ===============================
async function loadDomain(domain) {
  if (!domain.match(/^[a-zA-Z0-9_-]+$/)) {
    throw new Error("Invalid domain name");
  }

  const domainDir = path.join(RUNTIME_ROOT, domain);
  fs.mkdirSync(domainDir, { recursive: true });

  const { data: files, error } = await supabase
    .storage
    .from("prolog-files")
    .list(domain);

  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;

    const localPath = path.join(domainDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase
      .storage
      .from("prolog-files")
      .download(`${domain}/${file.name}`);

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
  }

  return domainDir;
}

// ===============================
// API: select domain (animals, etc.)
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  const { domain } = req.body;
  if (!domain) {
    return res.status(400).json({ error: "No domain provided" });
  }

  try {
    const dir = await loadDomain(domain);

    // казваме на Prolog къде е runtime директорията
    await sendToProlog(`set_runtime_dir('${dir.replace(/\\/g, "/")}')`);

    const helpText = await sendToProlog("help");

    res.json({
      message: `Domain '${domain}' loaded`,
      help: helpText
    });
  } catch (err) {
    console.error(err);
    res.status(500).json({ error: err.message });
  }
});

// ===============================
// API: send Prolog command
// ===============================
app.post("/prolog/command", async (req, res) => {
  const { command } = req.body;

  if (!command) {
    return res.status(400).json({ error: "No command provided" });
  }

  try {
    const output = await sendToProlog(command);
    res.json({ output });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// ===============================
// Health check
// ===============================
app.get("/", (req, res) => {
  res.send("🧠 Prolog API server is running");
});

// ===============================
app.listen(port, () => {
  console.log(`🚀 Server running on port ${port}`);
});
