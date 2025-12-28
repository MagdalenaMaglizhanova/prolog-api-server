// server.js
const express = require("express");
const cors = require("cors");
const path = require("path");
const fs = require("fs");
const { spawn } = require("child_process");
const { createClient } = require("@supabase/supabase-js");

const app = express();
const port = process.env.PORT || 10001;

app.use(cors());
app.use(express.json());

// ---------- Supabase ----------
const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

// ---------- Runtime ----------
let prolog = null;
let currentDomain = null;

// ---------- Helper: load domain ----------
async function loadDomain(domain) {
  if (!domain.match(/^[a-z]+$/)) {
    throw new Error("Invalid domain");
  }

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

    // презареждаме винаги (по-безопасно за development)
    const { data } = await supabase
      .storage
      .from("prolog-files")
      .download(`${domain}/${file.name}`);

    const buffer = Buffer.from(await data.arrayBuffer());
    fs.writeFileSync(localPath, buffer);
  }

  return path.join(baseDir, "main.pl");
}

// ---------- Start Prolog ----------
function startProlog(mainPl) {
  if (prolog) {
    prolog.kill();
    prolog = null;
  }

  prolog = spawn("swipl", ["-q"]);

  prolog.stdout.on("data", data => {
    console.log("PL:", data.toString());
  });

  prolog.stderr.on("data", data => {
    console.error("PL ERR:", data.toString());
  });

  const normalizedPath = mainPl.replace(/\\/g, "/");

  prolog.stdin.write(`consult('${normalizedPath}').\n`);
  prolog.stdin.write(`load_all.\n`);
}

// ---------- POST /prolog-run ----------
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;

  if (!query) return res.status(400).json({ error: "No query" });
  if (!domain) return res.status(400).json({ error: "No domain" });

  try {
    // ако домейнът се смени → рестарт на Prolog
    if (domain !== currentDomain) {
      const mainPl = await loadDomain(domain);
      startProlog(mainPl);
      currentDomain = domain;
    }

    let output = "";

    const onData = data => {
      output += data.toString();

      // КРАЙ на заявката (SWI-style)
      if (
        output.trim().endsWith("false") ||
        output.trim().endsWith("true")
      ) {
        prolog.stdout.off("data", onData);
        res.json({ result: output.trim() });
      }
    };

    prolog.stdout.on("data", onData);

    // махаме точка ако има и винаги ползваме run_all/1
    const cleanQuery = query.trim().replace(/\.$/, "");

    prolog.stdin.write(
      `run_all((${cleanQuery})).\n`
    );

  } catch (err) {
    console.error(err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`🔥 Prolog REPL server running on port ${port}`);
});
