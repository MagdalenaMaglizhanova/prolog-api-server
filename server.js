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
 * Load domain files from Supabase Storage
 * @param {string} domain
 * @returns {Promise<string>} path to main.pl
 */
async function loadDomain(domain) {
  if (!domain.match(/^[a-z]+$/)) {
    throw new Error("Invalid domain name. Use lowercase letters only.");
  }

  const baseDir = path.join(__dirname, RUNTIME_DIR, domain);
  fs.mkdirSync(baseDir, { recursive: true });

  const { data: files, error } = await supabase.storage.from(PROLOG_BUCKET).list(domain);
  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl") || file.name === 'temp_query.pl') continue; // skip temp files

    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase.storage.from(PROLOG_BUCKET).download(`${domain}/${file.name}`);
    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
    console.log(`Downloaded: ${file.name}`);
  }

  return path.join(baseDir, "main.pl");
}

/**
 * Create Prolog query file
 * @param {string} mainPlPath
 * @param {string} query
 * @param {string} domain
 * @returns {string} path to temp query file
 */
function createQueryFile(mainPlPath, query, domain) {
  const tmpFile = path.join(__dirname, RUNTIME_DIR, domain, "temp_query.pl");

  const safeQuery = query.endsWith('.') ? query : query + '.';

  const prologCode = `
:- consult('${mainPlPath.replace(/\\/g, "/")}').

run_query :-
    ${safeQuery},
    write('true'),
    nl.
  `;

  fs.writeFileSync(tmpFile, prologCode);
  return tmpFile;
}

/**
 * Execute Prolog query
 * @param {string} queryFile
 * @returns {Promise<{stdout: string, stderr: string}>}
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
 */
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!domain) return res.status(400).json({ error: "No domain specified" });

  try {
    const mainPlPath = await loadDomain(domain);
    const queryFile = createQueryFile(mainPlPath, query, domain);
    const { stdout, stderr } = await executePrologQuery(queryFile);

    res.json({
      result: stdout.trim() || "false",
      success: true
    });

    // optional cleanup
    // fs.unlinkSync(queryFile);

  } catch (err) {
    console.error("Server Error:", err);
    const errorMessage = err.stderr || err.message || err.error?.message;
    res.status(500).json({ error: errorMessage, success: false });
  }
});

/**
 * GET /health
 */
app.get("/health", (req, res) => {
  res.json({
    status: "ok",
    timestamp: new Date().toISOString(),
    service: "supabase-prolog-server"
  });
});

// Start server
app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
  console.log(`Runtime directory: ${path.join(__dirname, RUNTIME_DIR)}`);
});
