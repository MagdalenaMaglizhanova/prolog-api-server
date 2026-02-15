const express = require("express");
const cors = require("cors");
const { spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001;

app.use(cors());
app.use(express.json({ limit: '50mb' }));

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
// User sessions - отделен Prolog процес за всеки потребител
// ===============================
const userSessions = new Map(); // userId -> { prolog, buffer, domain, lastUsed }

// Функция за създаване на Prolog процес за потребител
function createPrologProcess(userId) {
  console.log(`[PROLOG][${userId}] Creating new Prolog process`);
  
  const prolog = spawn("swipl", [
    "-q",
    "-s",
    path.join(__dirname, "prolog", "main.pl")
  ], {
    encoding: 'utf8',
    env: { 
      ...process.env,
      LANG: 'en_US.UTF-8',
      LC_ALL: 'en_US.UTF-8'
    }
  });

  let stdoutBuffer = "";

  prolog.stdout.on("data", data => {
    stdoutBuffer += data.toString('utf8');
  });

  prolog.stderr.on("data", data => {
    console.error(`[PROLOG ERROR][${userId}]`, data.toString());
  });

  prolog.on("error", (err) => {
    console.error(`[PROLOG][${userId}] Process error:`, err);
  });

  prolog.on("exit", (code) => {
    console.log(`[PROLOG][${userId}] Process exited with code ${code}`);
    userSessions.delete(userId);
  });

  // Изчакваме малко за инициализация
  setTimeout(() => {
    // Инициализиране на сесията
    prolog.stdin.write(`init_session('${userId}').\n`);
    console.log(`[PROLOG][${userId}] Session initialized`);
  }, 500);

  return {
    prolog,
    buffer: stdoutBuffer,
    domain: null,
    lastUsed: Date.now()
  };
}

// Функция за изпращане на команда до потребителски Prolog процес
async function sendToProlog(userId, command, timeout = 5000) {
  let session = userSessions.get(userId);
  
  if (!session) {
    console.log(`[PROLOG][${userId}] No session found, creating new one`);
    session = createPrologProcess(userId);
    userSessions.set(userId, session);
    // Изчакваме инициализацията
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  // Обновяваме времето на последна употреба
  session.lastUsed = Date.now();
  
  return new Promise((resolve, reject) => {
    const cleanCommand = command.trim();
    console.log(`[PROLOG][${userId}] Sending command: "${cleanCommand}"`);
    
    // Изчистваме буфера
    session.buffer = "";
    
    // Изпращаме командата
    session.prolog.stdin.write(cleanCommand + ".\n");

    const start = Date.now();
    const interval = setInterval(() => {
      if (session.buffer.length > 0) {
        clearInterval(interval);
        console.log(`[PROLOG][${userId}] Response received (${session.buffer.length} bytes)`);
        
        const cleanedOutput = session.buffer.trim();
        resolve(cleanedOutput);
      }
      if (Date.now() - start > timeout) {
        clearInterval(interval);
        console.error(`[PROLOG][${userId}] Timeout after ${timeout}ms`);
        reject(new Error(`Prolog timeout after ${timeout}ms`));
      }
    }, 100);
  });
}

// Функция за изчистване на стари сесии
function cleanupOldSessions(maxAgeMs = 30 * 60 * 1000) { // 30 минути
  const now = Date.now();
  for (const [userId, session] of userSessions.entries()) {
    if (now - session.lastUsed > maxAgeMs) {
      console.log(`[CLEANUP] Removing inactive session for user ${userId}`);
      if (session.prolog && !session.prolog.killed) {
        session.prolog.stdin.write(`end_session.\n`);
        session.prolog.kill();
      }
      userSessions.delete(userId);
    }
  }
}

// Изчистваме стари сесии на всеки 10 минути
setInterval(cleanupOldSessions, 10 * 60 * 1000);

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
    // Ако няма файлове, създаваме празен .pl файл
    const emptyFile = path.join(domainDir, `${domain}.pl`);
    fs.writeFileSync(emptyFile, `% ${domain} domain\n% Add your Prolog facts here\n`);
    console.log(`[DOMAIN] Created empty file: ${emptyFile}`);
    return domainDir;
  }

  // Сваляне на всички .pl файлове
  let downloadedCount = 0;
  const plFiles = files.filter(f => f.name.endsWith('.pl'));
  
  console.log(`[DOWNLOAD] Found ${plFiles.length} Prolog files`);
  
  for (const file of plFiles) {
    console.log(`[DOWNLOAD] Processing: ${domain}/${file.name}`);
    
    const localPath = path.join(domainDir, file.name);
    
    try {
      // Проверка дали файлът вече съществува и е актуален
      if (fs.existsSync(localPath)) {
        const stats = fs.statSync(localPath);
        const fileAge = Date.now() - stats.mtimeMs;
        // Ако файлът е от последните 5 минути, прескачаме
        if (fileAge < 5 * 60 * 1000) {
          console.log(`[DOWNLOAD] File ${file.name} is recent, skipping download`);
          downloadedCount++;
          continue;
        }
      }

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

      // Записване на файла с UTF-8 кодиране
      const buffer = Buffer.from(await data.arrayBuffer());
      fs.writeFileSync(localPath, buffer, 'utf8');
      downloadedCount++;
      
      console.log(`[DOWNLOAD] ✓ Saved: ${file.name} (${buffer.length} bytes)`);
      
    } catch (err) {
      console.error(`[DOWNLOAD] Failed to process ${file.name}:`, err.message);
    }
  }

  if (downloadedCount === 0 && plFiles.length > 0) {
    console.log(`[DOMAIN] No new files downloaded, using existing files`);
  }

  // Проверка на файловете в директорията
  const dirFiles = fs.readdirSync(domainDir);
  console.log(`[DOMAIN] Files in directory: ${dirFiles.join(", ")}`);
  
  return domainDir;
}

// ===============================
// Helper: UTF-8 обработка на Prolog отговор
// ===============================
function processPrologOutput(output) {
  if (!output) return output;
  
  try {
    // Преобразуване на Unicode escape последователности
    let processed = output.replace(/\\u([0-9a-fA-F]{4})/g, (match, hex) => {
      return String.fromCharCode(parseInt(hex, 16));
    });
    
    // Премахване на излишни символи
    processed = processed.replace(/\\n/g, '\n').replace(/\\t/g, '\t');
    
    return processed;
  } catch (err) {
    console.warn(`[UTF8] Error processing output: ${err.message}`);
    return output;
  }
}

// ===============================
// API: Initialize session
// ===============================
app.post("/prolog/init-session", async (req, res) => {
  const { userId } = req.body;
  
  if (!userId) {
    return res.status(400).json({ error: "No userId provided" });
  }

  try {
    let session = userSessions.get(userId);
    
    if (!session) {
      session = createPrologProcess(userId);
      userSessions.set(userId, session);
      // Изчакваме инициализацията
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    session.lastUsed = Date.now();
    
    res.json({ 
      success: true, 
      message: "Session initialized",
      userId 
    });
  } catch (err) {
    console.error(`[API] Error initializing session:`, err);
    res.status(500).json({ error: err.message });
  }
});

// ===============================
// API: End session
// ===============================
app.post("/prolog/end-session", async (req, res) => {
  const { userId } = req.body;
  
  if (!userId) {
    return res.status(400).json({ error: "No userId provided" });
  }

  try {
    const session = userSessions.get(userId);
    
    if (session && session.prolog && !session.prolog.killed) {
      await sendToProlog(userId, "end_session");
      session.prolog.kill();
    }
    
    userSessions.delete(userId);
    
    res.json({ 
      success: true, 
      message: "Session ended" 
    });
  } catch (err) {
    console.error(`[API] Error ending session:`, err);
    res.status(500).json({ error: err.message });
  }
});

// ===============================
// API: List active sessions
// ===============================
app.get("/prolog/list-sessions", (req, res) => {
  const sessions = Array.from(userSessions.entries()).map(([userId, session]) => ({
    userId,
    domain: session.domain,
    lastUsed: new Date(session.lastUsed).toISOString(),
    active: session.prolog && !session.prolog.killed
  }));
  
  res.json({
    success: true,
    sessions,
    count: sessions.length
  });
});

// ===============================
// API: select domain (animals, etc.)
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  const { domain, userId } = req.body;
  console.log(`[API][${userId}] POST /prolog/select-domain for domain: "${domain}"`);
  
  if (!domain) {
    return res.status(400).json({ error: "No domain provided" });
  }

  if (!userId) {
    return res.status(400).json({ error: "No userId provided" });
  }

  try {
    // 1. Зареждане на домейна от Supabase
    console.log(`[API][${userId}] Step 1: Loading domain from Supabase...`);
    const dir = await loadDomain(domain);

    // 2. Конвертиране на пътя за Prolog (Unix стил)
    const prologPath = dir.replace(/\\/g, '/');
    console.log(`[API][${userId}] Step 2: Setting Prolog runtime dir to: "${prologPath}"`);

    // 3. Изчистване на старите файлове за този потребител
    console.log(`[API][${userId}] Step 3: Clearing old files...`);
    await sendToProlog(userId, 'unload_all').catch(() => {});

    // 4. Настройка на директорията в Prolog
    console.log(`[API][${userId}] Step 4: Configuring Prolog...`);
    const setDirResult = await sendToProlog(userId, `set_runtime_dir('${prologPath}')`);
    console.log(`[API][${userId}] Prolog set_runtime_dir response: ${setDirResult}`);

    // 5. Зареждане на всички файлове в Prolog
    console.log(`[API][${userId}] Step 5: Loading all Prolog files...`);
    const loadResult = await sendToProlog(userId, 'load_all');
    console.log(`[API][${userId}] Prolog load_all result: ${loadResult}`);

    // Обновяваме домейна в сесията
    const session = userSessions.get(userId);
    if (session) {
      session.domain = domain;
    }

    console.log(`[API][${userId}] Domain "${domain}" successfully loaded`);
    
    res.json({
      success: true,
      message: `Domain '${domain}' loaded successfully`,
      files: processPrologOutput(loadResult),
      directory: prologPath
    });

  } catch (err) {
    console.error(`[API][${userId}] Error loading domain "${domain}":`, err);
    
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
  const { command, userId } = req.body;
  console.log(`[API][${userId}] POST /prolog/command: "${command}"`);

  if (!command) {
    return res.status(400).json({ error: "No command provided" });
  }

  if (!userId) {
    return res.status(400).json({ error: "No userId provided" });
  }

  try {
    console.log(`[API][${userId}] Sending command to Prolog...`);
    const output = await sendToProlog(userId, command);
    const processedOutput = processPrologOutput(output);
    console.log(`[API][${userId}] Command executed successfully`);
    
    res.json({ 
      success: true,
      output: processedOutput
    });
  } catch (err) {
    console.error(`[API][${userId}] Error executing command:`, err);
    
    res.status(500).json({ 
      success: false,
      error: err.message || "Failed to execute Prolog command"
    });
  }
});

// ===============================
// API: Get user session info
// ===============================
app.get("/prolog/user-status/:userId", async (req, res) => {
  const { userId } = req.params;
  
  try {
    const session = userSessions.get(userId);
    
    if (!session) {
      return res.json({
        active: false,
        message: "No active session"
      });
    }

    // Проверка на текущия файл
    let currentFile = "unknown";
    try {
      currentFile = await sendToProlog(userId, "current_file");
    } catch (err) {
      currentFile = "error getting current file";
    }

    // Списък на заредените файлове
    let loadedFiles = [];
    try {
      const filesOutput = await sendToProlog(userId, "list_files");
      loadedFiles = filesOutput.split('\n').filter(line => line.includes('.pl'));
    } catch (err) {
      loadedFiles = [];
    }

    res.json({
      active: true,
      userId,
      domain: session.domain,
      lastUsed: new Date(session.lastUsed).toISOString(),
      currentFile: processPrologOutput(currentFile),
      loadedFiles
    });

  } catch (err) {
    console.error(`[API] Error getting user status:`, err);
    res.status(500).json({ error: err.message });
  }
});

// ===============================
// API: Check if domain is loaded
// ===============================
app.get("/prolog/status", async (req, res) => {
  try {
    console.log(`[API] GET /prolog/status - Checking Prolog status`);
    
    // Проверка на runtime директорията
    const runtimeExists = fs.existsSync(RUNTIME_ROOT);
    let runtimeContents = [];
    
    if (runtimeExists) {
      runtimeContents = fs.readdirSync(RUNTIME_ROOT);
    }
    
    res.json({
      success: true,
      server: {
        status: "running",
        port: port,
        activeSessions: userSessions.size,
        encoding: "utf8"
      },
      runtime: {
        exists: runtimeExists,
        path: RUNTIME_ROOT,
        contents: runtimeContents
      }
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
    encoding: "UTF-8",
    activeSessions: userSessions.size,
    endpoints: [
      "POST /prolog/init-session",
      "POST /prolog/end-session", 
      "GET /prolog/list-sessions",
      "POST /prolog/select-domain",
      "POST /prolog/command",
      "GET /prolog/user-status/:userId",
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
  
  // Убиваме всички Prolog процеси
  for (const [userId, session] of userSessions.entries()) {
    if (session.prolog && !session.prolog.killed) {
      session.prolog.stdin.write(`end_session.\n`);
      session.prolog.kill();
    }
  }
  
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('[SERVER] Received SIGINT, shutting down...');
  
  // Убиваме всички Prolog процеси
  for (const [userId, session] of userSessions.entries()) {
    if (session.prolog && !session.prolog.killed) {
      session.prolog.stdin.write(`end_session.\n`);
      session.prolog.kill();
    }
  }
  
  process.exit(0);
});

// ===============================
app.listen(port, () => {
  console.log(`🚀 Server running on port ${port}`);
  console.log(`📁 Runtime directory: ${RUNTIME_ROOT}`);
  console.log(`🌐 Health check: http://localhost:${port}/`);
  console.log(`📊 Status endpoint: http://localhost:${port}/prolog/status`);
  console.log(`🔄 UTF-8 encoding enabled`);
  console.log(`👥 Multi-user support enabled`);
});
