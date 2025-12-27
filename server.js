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

// Helper: зареждане на domain от Supabase
async function loadDomain(domain) {
  if (!domain.match(/^[a-z]+$/)) throw new Error("Invalid domain name");

  const baseDir = path.join(__dirname, "runtime", domain);
  fs.mkdirSync(baseDir, { recursive: true });

  const { data: files, error } = await supabase
    .storage
    .from("prolog-files")
    .list(domain);

  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;

    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase
      .storage
      .from("prolog-files")
      .download(`${domain}/${file.name}`);

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
  }

  return { mainPl: path.join(baseDir, "main.pl"), baseDir };
}

// POST /prolog-run
// Приема { query: "bird(X).", domain: "animals" }
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!domain) return res.status(400).json({ error: "No domain specified" });

  try {
    const { mainPl, baseDir } = await loadDomain(domain);

    const sanitizedQuery = query.trim().replace(/\.$/, ""); // махаме точката
    const tmpFile = path.join(baseDir, `temp_query_${Date.now()}.pl`);

    // Генерираме временно Prolog file, който събира решенията
    const tmpContent = `
:- working_directory(_, '${baseDir.replace(/\\/g,"/")}').
:- consult('${mainPl.replace(/\\/g, "/")}').
:- initialization(load_all).

run_query :-
    findall(${sanitizedQuery}, ${sanitizedQuery}, Results),
    ( Results = [] -> write('false'); write(Results) ),
    nl.
`;

    fs.writeFileSync(tmpFile, tmpContent);

    execFile("swipl", ["-q", "-s", tmpFile, "-g", "run_query", "-t", "halt"], (error, stdout, stderr) => {
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

// GET /prolog-files/:domain
app.get("/prolog-files/:domain", async (req, res) => {
  const { domain } = req.params;

  try {
    const { mainPl, baseDir } = await loadDomain(domain);

    const tmpFile = path.join(baseDir, `temp_help_${Date.now()}.pl`);
    const tmpContent = `
:- working_directory(_, '${baseDir.replace(/\\/g,"/")}').
:- consult('${mainPl.replace(/\\/g, "/")}').
:- initialization(load_all).

run_query :- help.
`;

    fs.writeFileSync(tmpFile, tmpContent);

    execFile("swipl", ["-q", "-s", tmpFile, "-g", "run_query", "-t", "halt"], (error, stdout, stderr) => {
      if (error) {
        console.error("Prolog Error:", error);
        return res.status(500).json({ error: stderr || error.message });
      }
      res.json({ result: stdout.trim() });
    });

  } catch (err) {
    console.error("Server Error:", err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
});
