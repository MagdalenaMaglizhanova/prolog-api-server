// server.js
const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001;

app.use(cors());
app.use(express.json());

// Supabase client
const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

// Helper: зареждане на папка/domain от Supabase
async function loadDomain(domain) {
  if (!domain.match(/^[a-z]+$/)) throw new Error("Invalid domain name");

  const baseDir = path.join(__dirname, "runtime", domain);
  fs.mkdirSync(baseDir, { recursive: true });

  const bucket = "prolog-files";
  const { data: files, error } = await supabase.storage.from(bucket).list(domain);

  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;
    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase.storage.from(bucket).download(`${domain}/${file.name}`);
    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer, { encoding: "utf8" });
  }

  return baseDir;
}

// POST /prolog-run
// Приема { query: "bird(X).", domain: "animals" }
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!domain) return res.status(400).json({ error: "No domain specified" });

  try {
    const domainPath = await loadDomain(domain);
    const mainPl = path.join(domainPath, "main.pl");
    if (!fs.existsSync(mainPl)) throw new Error("main.pl not found in domain");

    // Създаваме временен файл за query
    const tmpFile = path.join(domainPath, `temp_query.pl`);
    const tmpContent = `
      :- encoding(utf8).
      :- use_module('${mainPl}').
      run_query :- ${query}, write('true'), nl, halt.
    `;
    fs.writeFileSync(tmpFile, tmpContent, { encoding: "utf8" });

    execFile("swipl", ["-q", "-s", tmpFile, "-g", "run_query"], (error, stdout, stderr) => {
      if (error) {
        console.error("Prolog Error:", error);
        console.error("Prolog Stderr:", stderr);
        return res.status(500).json({ error: stderr || error.message });
      }
      res.json({ result: stdout.trim() || "false" });
    });

  } catch (err) {
    console.error("Server Error:", err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
});
