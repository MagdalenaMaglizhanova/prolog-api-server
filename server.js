// server.js - РАБОТЕЩА ВЕРСИЯ С UTF-8
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

console.log("[INIT] Supabase client initialized");

// ===============================
// Runtime directories
// ===============================
const RUNTIME_ROOT = path.join(__dirname, "runtime");
if (!fs.existsSync(RUNTIME_ROOT)) {
  fs.mkdirSync(RUNTIME_ROOT, { recursive: true });
  console.log(`[INIT] Created runtime directory: ${RUNTIME_ROOT}`);
} else {
  console.log(`[INIT] Runtime directory exists: ${RUNTIME_ROOT}`);
}

// ===============================
// Start persistent Prolog process WITH UTF-8
// ===============================
console.log("[INIT] Starting Prolog process with UTF-8...");

// КРИТИЧНО: Добавете --encoding=utf-8 но НЕ променяйте нищо друго!
const prolog = spawn("swipl", [
  "-q",
  "--encoding=utf-8",  // САМО ТОВА ДОБАВЕТЕ
  "-s",
  path.join(__dirname, "prolog", "main.pl")
]);

console.log("🧠 Prolog engine started with UTF-8 encoding");

prolog.stderr.on("data", data => {
  console.error("[PROLOG ERROR]", data.toString());
});

// Буфер за stdout
let stdoutBuffer = "";

prolog.stdout.on("data", data => {
  stdoutBuffer += data.toString();
});

// ===============================
// Helper: send command to Prolog
// ===============================
function sendToProlog(command, timeout = 5000) {
  return new Promise((resolve, reject) => {
    const cleanCommand = command.trim();
    console.log(`[PROLOG] Sending command: "${cleanCommand}"`);
    
    stdoutBuffer = "";
    prolog.stdin.write(cleanCommand + ".\n");

    const start = Date.now();
    const interval = setInterval(() => {
      if (stdoutBuffer.length > 0) {
        clearInterval(interval);
        console.log(`[PROLOG] Response length: ${stdoutBuffer.length} chars`);
        resolve(stdoutBuffer.trim());
      }
      if (Date.now() - start > timeout) {
        clearInterval(interval);
        console.error(`[PROLOG] Timeout after ${timeout}ms`);
        reject(new Error(`Prolog timeout after ${timeout}ms`));
      }
    }, 100);
  });
}

// ===============================
// Helper: load domain from Supabase WITH UTF-8
// ===============================
async function loadDomain(domain) {
  console.log(`[DOMAIN] Loading domain: "${domain}"`);
  
  // Разрешаваме кирилица в имена на домейни
  if (!domain.match(/^[a-zA-Zа-яА-Я0-9_-]+$/)) {
    throw new Error("Invalid domain name");
  }

  const domainDir = path.join(RUNTIME_ROOT, domain);
  console.log(`[DOMAIN] Target directory: ${domainDir}`);
  
  if (!fs.existsSync(domainDir)) {
    fs.mkdirSync(domainDir, { recursive: true });
    console.log(`[DOMAIN] Created directory: ${domainDir}`);
  } else {
    console.log(`[DOMAIN] Directory already exists: ${domainDir}`);
    
    const oldFiles = fs.readdirSync(domainDir);
    if (oldFiles.length > 0) {
      console.log(`[DOMAIN] Removing old files: ${oldFiles.join(", ")}`);
      for (const file of oldFiles) {
        try {
          fs.unlinkSync(path.join(domainDir, file));
        } catch (err) {
          console.warn(`[DOMAIN] Could not remove ${file}: ${err.message}`);
        }
      }
    }
  }

  console.log(`[SUPABASE] Listing files in bucket "prolog-files", folder "${domain}"`);
  const { data: files, error } = await supabase
    .storage
    .from("prolog-files")
    .list(domain);

  if (error) {
    console.error("[SUPABASE] Error listing files:", error);
    throw new Error(`Supabase error: ${error.message}`);
  }

  console.log(`[SUPABASE] Found ${files ? files.length : 0} files`);
  
  if (!files || files.length === 0) {
    throw new Error(`No files found for domain "${domain}" in Supabase`);
  }

  let downloadedCount = 0;
  const plFiles = files.filter(f => f.name.endsWith('.pl'));
  
  console.log(`[DOWNLOAD] Found ${plFiles.length} Prolog files`);
  
  for (const file of plFiles) {
    console.log(`[DOWNLOAD] Processing: ${domain}/${file.name}`);
    
    const localPath = path.join(domainDir, file.name);
    
    try {
      const { data, error: downloadError } = await supabase
        .storage
        .from("prolog-files")
        .download(`${domain}/${file.name}`);

      if (downloadError) {
        console.error(`[DOWNLOAD] Error downloading ${file.name}:`, downloadError);
        continue;
      }

      if (!data) {
        console.error(`[DOWNLOAD] No data received for ${file.name}`);
        continue;
      }

      const buffer = Buffer.from(await data.arrayBuffer());
      // КРИТИЧНО: Записваме с UTF-8 кодировка
      fs.writeFileSync(localPath, buffer, 'utf8');
      downloadedCount++;
      
      console.log(`[DOWNLOAD] ✓ Saved: ${file.name} (${buffer.length} bytes)`);
      
    } catch (err) {
      console.error(`[DOWNLOAD] Failed to process ${file.name}:`, err.message);
    }
  }

  if (downloadedCount === 0) {
    throw new Error(`No Prolog files could be downloaded for domain "${domain}"`);
  }

  console.log(`[DOMAIN] Successfully downloaded ${downloadedCount} files to ${domainDir}`);
  
  const downloadedFiles = fs.readdirSync(domainDir);
  console.log(`[DOMAIN] Files in directory: ${downloadedFiles.join(", ")}`);
  
  return domainDir;
}

// ===============================
// API endpoints (остават същите)
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  const { domain } = req.body;
  console.log(`[API] POST /prolog/select-domain for domain: "${domain}"`);
  
  if (!domain) {
    console.error("[API] No domain provided in request");
    return res.status(400).json({ error: "No domain provided" });
  }

  try {
    const dir = await loadDomain(domain);
    const prologPath = dir.replace(/\\/g, '/');
    
    console.log(`[API] Setting Prolog runtime dir to: "${prologPath}"`);
    const setDirResult = await sendToProlog(`set_runtime_dir('${prologPath}')`);
    console.log(`[API] Prolog set_runtime_dir response: ${setDirResult}`);

    const loadResult = await sendToProlog('load_all');
    console.log(`[API] Prolog load_all result: ${loadResult}`);

    const helpText = await sendToProlog("help");

    console.log(`[API] Domain "${domain}" successfully loaded`);
    
    res.json({
      success: true,
      message: `Domain '${domain}' loaded successfully`,
      files: loadResult,
      help: helpText,
      directory: prologPath
    });

  } catch (err) {
    console.error(`[API] Error loading domain "${domain}":`, err);
    
    res.status(500).json({ 
      success: false,
      error: `Failed to load domain "${domain}"`,
      details: err.message
    });
  }
});

app.post("/prolog/command", async (req, res) => {
  const { command } = req.body;
  console.log(`[API] POST /prolog/command: "${command}"`);

  if (!command) {
    return res.status(400).json({ error: "No command provided" });
  }

  try {
    const output = await sendToProlog(command);
    
    res.json({ 
      success: true,
      output: output
    });
  } catch (err) {
    console.error(`[API] Error executing command:`, err);
    
    res.status(500).json({ 
      success: false,
      error: err.message || "Failed to execute Prolog command"
    });
  }
});

app.get("/prolog/status", async (req, res) => {
  try {
    console.log(`[API] GET /prolog/status - Checking Prolog status`);
    
    const runtimeExists = fs.existsSync(RUNTIME_ROOT);
    let runtimeContents = [];
    
    if (runtimeExists) {
      runtimeContents = fs.readdirSync(RUNTIME_ROOT);
    }
    
    let prologStatus = "Prolog not responding";
    try {
      prologStatus = await sendToProlog("current_file");
    } catch (err) {
      prologStatus = `Prolog error: ${err.message}`;
    }
    
    res.json({
      success: true,
      server: {
        status: "running",
        port: port
      },
      runtime: {
        exists: runtimeExists,
        path: RUNTIME_ROOT,
        contents: runtimeContents
      },
      prolog: prologStatus
    });
    
  } catch (err) {
    console.error("[API] Error checking status:", err);
    res.status(500).json({ error: err.message });
  }
});

app.get("/", (req, res) => {
  res.json({
    service: "Prolog API Server",
    status: "running",
    version: "1.0.0",
    encoding: "UTF-8",
    endpoints: [
      "POST /prolog/select-domain",
      "POST /prolog/command", 
      "GET /prolog/status"
    ]
  });
});

app.listen(port, () => {
  console.log(`🚀 Server running on port ${port}`);
  console.log(`📁 Runtime directory: ${RUNTIME_ROOT}`);
  console.log(`🔤 Encoding: UTF-8`);
});
