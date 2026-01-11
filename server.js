// server.js - Коригирана версия с пълна поддръжка на кирилица

const express = require("express");
const cors = require("cors");
const { spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001;

// ===============================
// Middleware с правилна кодировка
// ===============================
app.use(cors());
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true, limit: '50mb' }));

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
// Start persistent Prolog process with UTF-8 encoding
// ===============================
console.log("[INIT] Starting Prolog process with UTF-8 support...");

const prolog = spawn("swipl", [
  "-q",
  "--encoding=utf-8",  // КРИТИЧНО: Задаване на кодировка
  "-s",
  path.join(__dirname, "prolog", "main.pl")
], {
  env: {
    ...process.env,
    LANG: 'en_US.UTF-8',  // КРИТИЧНО: Задаване на локали
    LC_ALL: 'en_US.UTF-8' // КРИТИЧНО: Задаване на всички локали
  }
});

// КРИТИЧНО: Задаване на кодировки за всички потоци
prolog.stdin.setDefaultEncoding('utf8');
prolog.stdout.setEncoding('utf8');
prolog.stderr.setEncoding('utf8');

console.log("🧠 Prolog engine started with UTF-8 encoding");

// Буфер за stdout
let stdoutBuffer = "";

prolog.stdout.on("data", data => {
  stdoutBuffer += data;
  // Дебъг за кирилица
  if (data.includes('а') || data.includes('б') || data.includes('в')) {
    console.log(`[DEBUG] Cyrillic detected in output: ${data.substring(0, 100)}`);
  }
});

prolog.stderr.on("data", data => {
  console.error("[PROLOG ERROR]", data.toString());
});

// ===============================
// Helper: send command to Prolog with encoding
// ===============================
function sendToProlog(command, timeout = 5000) {
  return new Promise((resolve, reject) => {
    const cleanCommand = command.trim();
    console.log(`[PROLOG] Sending command: "${cleanCommand}"`);
    
    // Проверка за кирилица в командата
    if (/[а-яА-Я]/.test(cleanCommand)) {
      console.log(`[DEBUG] Command contains Cyrillic: ${cleanCommand}`);
    }
    
    stdoutBuffer = "";
    
    // КРИТИЧНО: Записване на командата с правилното кодиране
    const success = prolog.stdin.write(cleanCommand + ".\n", 'utf8');
    
    if (!success) {
      console.error("[PROLOG] Failed to write to stdin");
      reject(new Error("Failed to write to Prolog stdin"));
      return;
    }
    
    const start = Date.now();
    const interval = setInterval(() => {
      if (stdoutBuffer.length > 0 && !stdoutBuffer.endsWith('\n')) {
        clearInterval(interval);
        
        // КРИТИЧНО: Нормализиране на кирилицата
        const normalizedOutput = stdoutBuffer
          .trim()
          .normalize('NFC'); // Нормализиране на Unicode
        
        console.log(`[PROLOG] Response length: ${normalizedOutput.length} chars`);
        console.log(`[PROLOG] First 200 chars: ${normalizedOutput.substring(0, 200)}...`);
        
        // Проверка за валиден UTF-8
        const isValidUTF8 = Buffer.from(normalizedOutput, 'utf8').toString('utf8') === normalizedOutput;
        console.log(`[PROLOG] UTF-8 valid: ${isValidUTF8}`);
        
        resolve(normalizedOutput);
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
// Helper: load domain from Supabase with encoding
// ===============================
async function loadDomain(domain) {
  console.log(`[DOMAIN] Loading domain: "${domain}"`);
  
  // Валидация - разрешаваме кирилица за имена на домейни
  if (!domain.match(/^[a-zA-Zа-яА-Я0-9_-]+$/)) {
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

      // Преобразуване в буфер и записване с UTF-8 кодировка
      const buffer = Buffer.from(await data.arrayBuffer());
      
      // КРИТИЧНО: Записване с UTF-8 кодировка
      fs.writeFileSync(localPath, buffer, 'utf8');
      downloadedCount++;
      
      // Проверка за кирилица в съдържанието
      const content = buffer.toString('utf8');
      const hasCyrillic = /[а-яА-Я]/.test(content);
      
      console.log(`[DOWNLOAD] ✓ Saved: ${file.name} (${buffer.length} bytes)`);
      if (hasCyrillic) {
        console.log(`[DOWNLOAD] ℹ️ File contains Cyrillic characters`);
      }
      
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
// API: select domain
// ===============================
app.post("/prolog/select-domain", async (req, res) => {
  // КРИТИЧНО: Задаване на кодировка за отговора
  res.setHeader("Content-Type", "application/json; charset=utf-8");
  
  const { domain } = req.body;
  console.log(`[API] POST /prolog/select-domain for domain: "${domain}"`);
  
  if (!domain) {
    console.error("[API] No domain provided in request");
    return res.status(400).json({ error: "No domain provided" });
  }

  try {
    // Зареждане на домейна
    const dir = await loadDomain(domain);

    // Конвертиране на пътя
    const prologPath = dir.replace(/\\/g, '/');
    console.log(`[API] Setting Prolog runtime dir to: "${prologPath}"`);

    // Настройка на директорията в Prolog
    const setDirResult = await sendToProlog(`set_runtime_dir('${prologPath}')`);
    console.log(`[API] Prolog set_runtime_dir response: ${setDirResult}`);

    // Зареждане на всички файлове
    const loadResult = await sendToProlog('load_all');
    console.log(`[API] Prolog load_all result: ${loadResult}`);

    // Взимане на помощния текст
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
  // КРИТИЧНО: Задаване на кодировка за отговора
  res.setHeader("Content-Type", "application/json; charset=utf-8");
  
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

// ===============================
// Също трябва да промените prolog/main.pl файла:
// ===============================
/*
% main.pl - Prolog файл с UTF-8 поддръжка

:- encoding('UTF-8').

:- dynamic runtime_dir/1.
runtime_dir('.').

set_runtime_dir(Dir) :-
    retractall(runtime_dir(_)),
    assertz(runtime_dir(Dir)),
    format('Runtime directory set to: ~w', [Dir]).

load_all :-
    runtime_dir(Dir),
    atom_concat(Dir, '/*.pl', Pattern),
    expand_file_name(Pattern, Files),
    maplist(consult, Files),
    length(Files, Count),
    format('Loaded ~w files', [Count]).

help :-
    writeln('Available commands:'),
    writeln('  animal(X). - find animals'),
    writeln('  mammal(X). - find mammals'),
    writeln('  bird(X). - find birds'),
    writeln('  list_animals. - list all animals'),
    writeln('  clear_facts. - clear all facts').

% Пример за кирилица:
% животно(куче).
% животно(котка).
% животно(кон).
*/

// ===============================
// Стартиране на сървъра
// ===============================
app.listen(port, () => {
  console.log(`🚀 Server running on port ${port}`);
  console.log(`📁 Runtime directory: ${RUNTIME_ROOT}`);
  console.log(`🌐 UTF-8 Encoding: ENABLED`);
  console.log(`🔤 Locale: ${process.env.LANG || 'not set'}`);
});
