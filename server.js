// server.js
const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");
const fs = require("fs");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001; // друг порт, за да не се бърка със стария

app.use(cors());
app.use(express.json());

// Supabase client
const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

// Helper: зареждане на domain от Supabase
async function loadDomain(domain) {
  if (!domain.match(/^[a-z]+$/)) throw new Error("Invalid domain name");

  const baseDir = path.join(__dirname, "runtime", domain);
  fs.mkdirSync(baseDir, { recursive: true });

  // Вземаме списъка с файлове
  const { data: files, error } = await supabase
    .storage
    .from("prolog-kb")
    .list(domain);

  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;

    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase
      .storage
      .from("prolog-kb")
      .download(`${domain}/${file.name}`);

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
  }

  // Връщаме пътя към main.pl
  return path.join(baseDir, "main.pl");
}

// POST /prolog-run
// Приема { query: "bird(X).", domain: "animal" }
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) {
    return res.status(400).json({ error: "No query provided" });
  }

  try {
    // Зареждаме domain
    let prologFiles = [];
    if (domain) {
      const mainPl = await loadDomain(domain);
      prologFiles.push(mainPl);
    } else {
      return res.status(400).json({ error: "No domain specified" });
    }

    // Създаваме временен файл с query
    const tmpFile = path.join(__dirname, "runtime", domain, "temp_query.pl");
    fs.writeFileSync(tmpFile, `:- initialization(main).\nmain :- ${query}, write('true'), nl, halt.`);

    prologFiles.push(tmpFile);

    // Стартираме SWI-Prolog
    execFile("swipl", ["-q", ...prologFiles.flatMap(f => ["-s", f]), "-g", "main"], (error, stdout, stderr) => {
      if (error) {
        console.error("Prolog Error:", error);
        console.error("Prolog Stderr:", stderr);
        res.status(500).json({ error: stderr || error.message });
      } else {
        res.json({ result: stdout.trim() || "false" });
      }
    });
  } catch (err) {
    console.error("Server Error:", err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
});
