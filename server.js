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

  // Вземаме списъка с файлове от Supabase bucket "prolog-kb"
  const { data: files, error } = await supabase.storage.from("prolog-files").list(domain);
  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;
    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data, error: downloadErr } = await supabase
      .storage
      .from("prolog-files")
      .download(`${domain}/${file.name}`);

    if (downloadErr) throw downloadErr;

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
  }

  // Връщаме пътя към main.pl
  const mainPlPath = path.join(baseDir, "main.pl");
  if (!fs.existsSync(mainPlPath)) throw new Error("main.pl not found in domain");
  return mainPlPath;
}

// POST /prolog-run
// Приема { query: "bird(X).", domain: "animal" }
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });
  if (!domain) return res.status(400).json({ error: "No domain specified" });

  try {
    const mainPl = await loadDomain(domain);

    // Създаваме временен файл с wrapper за query
    const tmpQueryFile = path.join(__dirname, "runtime", domain, "temp_query.pl");
    fs.writeFileSync(
      tmpQueryFile,
      `
:- initialization(main).

main :-
    (${query}),
    write('true'), nl,
    halt.
`
    );

    // Стартираме SWI-Prolog с main.pl + temp query
    execFile(
      "swipl",
      ["-q", "-s", mainPl, "-s", tmpQueryFile, "-g", "main"],
      (error, stdout, stderr) => {
        if (error) {
          console.error("Prolog Error:", error);
          console.error("Prolog Stderr:", stderr);
          return res.status(500).json({ error: stderr || error.message });
        }
        res.json({ result: stdout.trim() || "false" });
      }
    );
  } catch (err) {
    console.error("Server Error:", err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`Supabase Prolog server running on port ${port}`);
});
