// server.js
const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const { createClient } = require("@supabase/supabase-js");

// Initialize Express app
const app = express();
const port = process.env.PORT || 10001;

// Middleware
app.use(cors());
app.use(express.json());

// Supabase client initialization
const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

// Constants
const RUNTIME_DIR = "runtime";
const PROLOG_BUCKET = "prolog-files";

/**
 * Зарежда файловете за даден domain от Supabase Storage
 * @param {string} domain - Името на домейна
 * @returns {Promise<string>} Път към main.pl файла
 */
async function loadDomain(domain) {
  // Валидация на името на домейна
  if (!domain.match(/^[a-z]+$/)) {
    throw new Error("Invalid domain name. Use lowercase letters only.");
  }

  // Създаване на директория за домейна
  const baseDir = path.join(__dirname, RUNTIME_DIR, domain);
  fs.mkdirSync(baseDir, { recursive: true });

  // Взимане на списък с файлове от Supabase Storage
  const { data: files, error } = await supabase
    .storage
    .from(PROLOG_BUCKET)
    .list(domain);

  if (error) {
    throw error;
  }

  // Сваляне на всеки .pl файл
  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;

    const localPath = path.join(baseDir, file.name);
    
    // Пропускане ако файлът вече съществува
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase
      .storage
      .from(PROLOG_BUCKET)
      .download(`${domain}/${file.name}`);

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
    console.log(`Downloaded: ${file.name}`);
  }

  // Връщане на пътя към main.pl
  return path.join(baseDir, "main.pl");
}

/**
 * Създава Prolog query файл
 * @param {string} mainPlPath - Път към main.pl
 * @param {string} query - Prolog заявка
 * @param {string} domain - Име на домейна
 * @returns {string} Път към временния файл
 */
function createQueryFile(mainPlPath, query, domain) {
  const tmpFile = path.join(__dirname, RUNTIME_DIR, domain, "temp_query.pl");
  
  const prologCode = `
:- consult('${mainPlPath.replace(/\\/g, "/")}').

run_query :-
    ${query},
    write('true'),
    nl.
  `;

  fs.writeFileSync(tmpFile, prologCode);
  return tmpFile;
}

/**
 * Изпълнява Prolog заявка
 * @param {string} queryFile - Път към query файла
 * @returns {Promise<{stdout: string, stderr: string}>} Резултат от изпълнението
 */
function executePrologQuery(queryFile) {
  return new Promise((resolve, reject) => {
    execFile(
      "swipl",
      ["-q", "-s", queryFile, "-g", "run_query", "-t", "halt"],
      (error, stdout, stderr) => {
        if (error) {
          reject({ error, stderr });
        } else {
          resolve({ stdout, stderr });
        }
      }
    );
  });
}

/**
 * POST /prolog-run
 * Изпълнява Prolog заявка
 * Body: { query: "bird(X).", domain: "animals" }
 */
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  // Валидация на входните данни
  if (!query) {
    return res.status(400).json({ error: "No query provided" });
  }

  if (!domain) {
    return res.status(400).json({ error: "No domain specified" });
  }

  try {
    // Зареждане на файловете за домейна
    const mainPlPath = await loadDomain(domain);
    
    // Създаване на временен файл с заявката
    const queryFile = createQueryFile(mainPlPath, query, domain);
    
    // Изпълнение на Prolog заявката
    const { stdout, stderr } = await executePrologQuery(queryFile);
    
    // Връщане на резултата
    res.json({
      result: stdout.trim() || "false",
      success: true
    });

    // Почистване на временния файл (по желание)
    // fs.unlinkSync(queryFile);

  } catch (err) {
    console.error("Server Error:", err);
    
    const errorMessage = err.stderr || err.message || err.error?.message;
    res.status(500).json({
      error: errorMessage,
      success: false
    });
  }
});

/**
 * GET /health
 * Проверка за живост на сървъра
 */
app.get("/health", (req, res) => {
  res.json({
    status: "ok",
    timestamp: new Date().toISOString(),
    service: "supabase-prolog-server"
  });
});

// Стартиране на сървъра
app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
  console.log(`Runtime directory: ${path.join(__dirname, RUNTIME_DIR)}`);
});
