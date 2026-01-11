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
        console.log(`[PROLOG] Response: ${stdoutBuffer.substring(0, 200)}...`);
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
// Helper: load domain from Supabase
// ===============================
async function loadDomain(domain) {
  console.log(`[DOMAIN] Loading domain: "${domain}"`);
  
  // Валидация на името на домейна
  if (!domain.match(/^[a-zA-Z0-9_-]+$/)) {
    throw new Error("Invalid domain name");
  }

  // Създаване на директория за домейна
  const domainDir = path.join(RUNTIME_ROOT, domain);
  console.log(`[DOMAIN] Target directory: ${domainDir}`);
  
  if (!fs.existsSync(domainDir)) {
    fs.mkdirSync(domainDir, { recursive: true });
    console.log(`[DOMAIN] Created directory: ${domainDir}`);
  } else {
    console.log(`[DOMAIN] Directory already exists: ${domainDir}`);
    
    // Изчистване на стари файлове преди ново сваляне
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

  // Извличане на списък с файлове от Supabase
  console.log(`[SUPABASE] Listing files in bucket "prolog-files", folder "${domain}"`);
  const { data: files, error } = await supabase
    .storage
    .from("prolog-files")
    .list(domain);

  if (error) {
    console.error("[SUPABASE] Error listing files:", error);
    throw new Error(`Supabase error: ${error.message}`);
  }

  console.log(`[SUPABASE] Found ${files ? files.length : 0} files:`, 
    files ? files.map(f => f.name).join(", ") : "none");

  if (!files || files.length === 0) {
    throw new Error(`No files found for domain "${domain}" in Supabase`);
  }

  // Сваляне на всички .pl файлове
  let downloadedCount = 0;
  const plFiles = files.filter(f => f.name.endsWith('.pl'));
  
  console.log(`[DOWNLOAD] Found ${plFiles.length} Prolog files`);
  
  for (const file of plFiles) {
    console.log(`[DOWNLOAD] Processing: ${domain}/${file.name}`);
    
    const localPath = path.join(domainDir, file.name);
    
    try {
      // Сваляне на файла от Supabase
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

      // Записване на файла
      const buffer = Buffer.from(await data.arrayBuffer());
      fs.writeFileSync(localPath, buffer);
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
  
  // Проверка на сваляните файлове
  const downloadedFiles = fs.readdirSync(domainDir);
  console.log(`[DOMAIN] Files in directory: ${downloadedFiles.join(", ")}`);
  
  return domainDir;
}

// ===============================
// API: select domain (animals, etc.)
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  const { domain } = req.body;
  console.log(`[API] POST /prolog/select-domain for domain: "${domain}"`);
  
  if (!domain) {
    console.error("[API] No domain provided in request");
    return res.status(400).json({ error: "No domain provided" });
  }

  try {
    // 1. Зареждане на домейна от Supabase
    console.log(`[API] Step 1: Loading domain from Supabase...`);
    const dir = await loadDomain(domain);

    // 2. Конвертиране на пътя за Prolog (Unix стил)
    const prologPath = dir.replace(/\\/g, '/');
    console.log(`[API] Step 2: Setting Prolog runtime dir to: "${prologPath}"`);

    // 3. Настройка на директорията в Prolog
    console.log(`[API] Step 3: Configuring Prolog...`);
    const setDirResult = await sendToProlog(`set_runtime_dir('${prologPath}')`);
    console.log(`[API] Prolog set_runtime_dir response: ${setDirResult}`);

    // 4. Зареждане на всички файлове в Prolog
    console.log(`[API] Step 4: Loading all Prolog files...`);
    const loadResult = await sendToProlog('load_all');
    console.log(`[API] Prolog load_all result: ${loadResult}`);

    // 5. Взимане на помощния текст
    console.log(`[API] Step 5: Getting help...`);
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
    
    // Детайлна грешка
    const errorMessage = err.message || "Unknown error";
    const errorStack = err.stack || "No stack trace";
    
    console.error(`[API] Error details: ${errorMessage}`);
    console.error(`[API] Stack trace: ${errorStack}`);
    
    res.status(500).json({ 
      success: false,
      error: `Failed to load domain "${domain}"`,
      details: errorMessage,
      stack: process.env.NODE_ENV === 'development' ? errorStack : undefined
    });
  }
});

// ===============================
// API: send Prolog command
// ===============================
app.post("/prolog/command", async (req, res) => {
  const { command } = req.body;
  console.log(`[API] POST /prolog/command: "${command}"`);

  if (!command) {
    return res.status(400).json({ error: "No command provided" });
  }

  try {
    console.log(`[API] Sending command to Prolog...`);
    const output = await sendToProlog(command);
    console.log(`[API] Command executed successfully`);
    
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

// ===============================
// API: Check if domain is loaded
// ===============================
app.get("/prolog/status", async (req, res) => {
  try {
    console.log(`[API] GET /prolog/status - Checking Prolog status`);
    
    // Проверка дали Prolog процесът работи
    const isPrologAlive = prolog && !prolog.killed;
    
    // Проверка на runtime директорията
    const runtimeExists = fs.existsSync(RUNTIME_ROOT);
    let runtimeContents = [];
    
    if (runtimeExists) {
      runtimeContents = fs.readdirSync(RUNTIME_ROOT);
    }
    
    // Проверка на текущия файл в Prolog
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
        port: port,
        prologProcess: isPrologAlive ? "alive" : "dead"
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

// ===============================
// Health check
// ===============================
app.get("/", (req, res) => {
  res.json({
    service: "Prolog API Server",
    status: "running",
    version: "1.0.0",
    endpoints: [
      "POST /prolog/select-domain",
      "POST /prolog/command", 
      "GET /prolog/status"
    ]
  });
});

// ===============================
// Error handling middleware
// ===============================
app.use((err, req, res, next) => {
  console.error(`[ERROR] Unhandled error:`, err);
  res.status(500).json({ 
    error: "Internal server error",
    message: err.message 
  });
});

// ===============================
// Handle process termination
// ===============================
process.on('SIGTERM', () => {
  console.log('[SERVER] Received SIGTERM, shutting down...');
  if (prolog && !prolog.killed) {
    prolog.kill();
  }
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('[SERVER] Received SIGINT, shutting down...');
  if (prolog && !prolog.killed) {
    prolog.kill();
  }
  process.exit(0);
});

// ===============================
app.listen(port, () => {
  console.log(`🚀 Server running on port ${port}`);
  console.log(`📁 Runtime directory: ${RUNTIME_ROOT}`);
  console.log(`🌐 Health check: http://localhost:${port}/`);
  console.log(`📊 Status endpoint: http://localhost:${port}/prolog/status`);
});
