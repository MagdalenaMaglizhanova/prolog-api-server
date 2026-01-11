// server.js - Върнете се към работещата версия с подобрения за кирилица
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
// Start persistent Prolog process - FIXED VERSION
// ===============================
let prolog;
let isPrologAlive = false;

function startPrologProcess() {
  console.log("[PROLOG] Starting Prolog process...");
  
  // КРИТИЧНО: Стартираме Prolog с UTF-8 опции
  prolog = spawn("swipl", [
    "-q",
    "--encoding=utf-8",  // Добавяме кодировка
    "-s",
    path.join(__dirname, "prolog", "main.pl")
  ], {
    env: {
      ...process.env,
      LANG: 'en_US.UTF-8',
      LC_ALL: 'en_US.UTF-8'
    }
  });

  // Настройка на кодировката
  prolog.stdout.setEncoding("utf8");
  prolog.stderr.setEncoding("utf8");
  
  console.log("🧠 Prolog engine started with UTF-8 encoding");

  prolog.stdout.on("data", data => {
    console.log("[PROLOG STDOUT]", data.substring(0, 200));
  });

  prolog.stderr.on("data", data => {
    console.error("[PROLOG STDERR]", data.toString());
  });

  prolog.on("error", err => {
    console.error("[PROLOG PROCESS ERROR]", err);
    isPrologAlive = false;
  });

  prolog.on("exit", (code, signal) => {
    console.log(`[PROLOG] Process exited with code ${code}, signal ${signal}`);
    isPrologAlive = false;
    
    // Опит за рестарт след 2 секунди
    setTimeout(() => {
      console.log("[PROLOG] Attempting to restart...");
      startPrologProcess();
    }, 2000);
  });

  prolog.on("close", (code) => {
    console.log(`[PROLOG] Process closed with code ${code}`);
    isPrologAlive = false;
  });

  isPrologAlive = true;
  
  // Проверка дали процесът работи
  setTimeout(() => {
    if (prolog && !prolog.killed) {
      console.log("[PROLOG] Process is alive and running");
    } else {
      console.error("[PROLOG] Process failed to start");
    }
  }, 1000);
}

// Стартираме Prolog процеса
startPrologProcess();

// Буфер за stdout
let stdoutBuffer = "";

prolog.stdout.on("data", data => {
  stdoutBuffer += data.toString();
});

// ===============================
// Helper: send command to Prolog - IMPROVED
// ===============================
function sendToProlog(command, timeout = 5000) {
  return new Promise((resolve, reject) => {
    if (!prolog || prolog.killed || !isPrologAlive) {
      console.error("[PROLOG] Process is dead, cannot send command");
      reject(new Error("Prolog process is not running"));
      return;
    }

    const cleanCommand = command.trim();
    console.log(`[PROLOG] Sending command: "${cleanCommand}"`);
    
    stdoutBuffer = "";
    
    // Проверка дали stdin е достъпен
    if (!prolog.stdin.writable) {
      console.error("[PROLOG] stdin is not writable");
      reject(new Error("Prolog stdin is not writable"));
      return;
    }

    try {
      // Изпращаме командата с нова линия
      const success = prolog.stdin.write(cleanCommand + ".\n");
      
      if (!success) {
        console.error("[PROLOG] Write returned false");
        reject(new Error("Failed to write to Prolog stdin"));
        return;
      }
      
      console.log(`[PROLOG] Command written successfully: "${cleanCommand}"`);

      const start = Date.now();
      const interval = setInterval(() => {
        // Проверяваме дали имаме отговор
        if (stdoutBuffer.length > 0 && !stdoutBuffer.endsWith('\n')) {
          clearInterval(interval);
          console.log(`[PROLOG] Got response of ${stdoutBuffer.length} chars`);
          resolve(stdoutBuffer.trim());
        }
        
        // Проверка за timeout
        if (Date.now() - start > timeout) {
          clearInterval(interval);
          console.error(`[PROLOG] Timeout after ${timeout}ms, buffer: "${stdoutBuffer}"`);
          reject(new Error(`Prolog timeout after ${timeout}ms`));
        }
      }, 100);
      
    } catch (err) {
      console.error("[PROLOG] Exception during write:", err);
      reject(new Error(`Failed to write command: ${err.message}`));
    }
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

  const domainDir = path.join(RUNTIME_ROOT, domain);
  console.log(`[DOMAIN] Target directory: ${domainDir}`);
  
  if (!fs.existsSync(domainDir)) {
    fs.mkdirSync(domainDir, { recursive: true });
    console.log(`[DOMAIN] Created directory: ${domainDir}`);
  } else {
    console.log(`[DOMAIN] Directory already exists: ${domainDir}`);
    
    // Изчистване на стари файлове
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

  console.log(`[SUPABASE] Found ${files ? files.length : 0} files`);
  
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
      // КРИТИЧНО: Записване с UTF-8 кодировка
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
// API: select domain
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  const { domain } = req.body;
  console.log(`[API] POST /prolog/select-domain for domain: "${domain}"`);
  
  if (!domain) {
    console.error("[API] No domain provided in request");
    return res.status(400).json({ error: "No domain provided" });
  }

  try {
    // Проверка дали Prolog процесът работи
    if (!isPrologAlive || !prolog || prolog.killed) {
      console.error("[API] Prolog process is not running, restarting...");
      startPrologProcess();
      await new Promise(resolve => setTimeout(resolve, 1000));
    }

    // 1. Зареждане на домейна от Supabase
    console.log(`[API] Step 1: Loading domain from Supabase...`);
    const dir = await loadDomain(domain);

    // 2. Конвертиране на пътя
    const prologPath = dir.replace(/\\/g, '/');
    console.log(`[API] Step 2: Setting Prolog runtime dir to: "${prologPath}"`);

    // 3. Настройка на директорията в Prolog
    console.log(`[API] Step 3: Configuring Prolog...`);
    const setDirResult = await sendToProlog(`set_runtime_dir('${prologPath}')`);
    console.log(`[API] Prolog set_runtime_dir response: ${setDirResult}`);

    // 4. Зареждане на всички файлове
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
    
    res.status(500).json({ 
      success: false,
      error: `Failed to load domain "${domain}"`,
      details: err.message
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
    if (!isPrologAlive || !prolog || prolog.killed) {
      throw new Error("Prolog process is not running");
    }
    
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

// ===============================
// API: Check if domain is loaded
// ===============================
app.get("/prolog/status", async (req, res) => {
  try {
    console.log(`[API] GET /prolog/status - Checking Prolog status`);
    
    const runtimeExists = fs.existsSync(RUNTIME_ROOT);
    let runtimeContents = [];
    
    if (runtimeExists) {
      runtimeContents = fs.readdirSync(RUNTIME_ROOT);
    }
    
    // Проверка на текущия файл в Prolog
    let prologStatus = "Prolog not responding";
    let prologAlive = false;
    
    if (isPrologAlive && prolog && !prolog.killed) {
      try {
        prologStatus = await sendToProlog("current_file");
        prologAlive = true;
      } catch (err) {
        prologStatus = `Prolog error: ${err.message}`;
        prologAlive = false;
      }
    }
    
    res.json({
      success: true,
      server: {
        status: "running",
        port: port,
        prologProcess: prologAlive ? "alive" : "dead",
        isPrologAlive: isPrologAlive,
        prologProcessExists: !!prolog,
        prologKilled: prolog ? prolog.killed : true
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
// API: Restart Prolog process
// ===============================
app.post("/prolog/restart", (req, res) => {
  console.log("[API] POST /prolog/restart - Restarting Prolog process");
  
  try {
    if (prolog && !prolog.killed) {
      prolog.kill('SIGTERM');
      console.log("[API] Sent SIGTERM to existing Prolog process");
    }
    
    startPrologProcess();
    
    res.json({
      success: true,
      message: "Prolog process restart initiated"
    });
    
  } catch (err) {
    console.error("[API] Error restarting Prolog:", err);
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
    prologAlive: isPrologAlive,
    endpoints: [
      "POST /prolog/select-domain",
      "POST /prolog/command", 
      "GET /prolog/status",
      "POST /prolog/restart"
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
  console.log(`🔄 Restart endpoint: http://localhost:${port}/prolog/restart`);
});
