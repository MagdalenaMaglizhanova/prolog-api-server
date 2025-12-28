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

const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_ANON_KEY
);

let prolog = null;
let currentDomain = null;

// ---------- Helper: load domain ----------
async function loadDomain(domain) {
  if (!domain.match(/^[a-z_]+$/)) throw new Error("Invalid domain");

  const baseDir = path.join(__dirname, "runtime", domain);
  fs.mkdirSync(baseDir, { recursive: true });

  const { data: files, error } = await supabase.storage.from("prolog-files").list(domain);
  if (error) throw error;

  for (const file of files) {
    if (!file.name.endsWith(".pl")) continue;
    const localPath = path.join(baseDir, file.name);
    if (fs.existsSync(localPath)) continue;

    const { data } = await supabase.storage.from("prolog-files").download(`${domain}/${file.name}`);
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

  prolog.stdin.write(`consult('${mainPl.replace(/\\/g, "/")}').\n`);
  prolog.stdin.write(`init('${path.dirname(mainPl).replace(/\\/g, "/")}').\n`);
}

// ---------- POST /prolog-run ----------
app.post("/prolog-run", async (req, res) => {
  const { query, domain } = req.body;
  if (!query) return res.status(400).json({ error: "No query" });
  if (!domain) return res.status(400).json({ error: "No domain" });

  try {
    if (domain !== currentDomain) {
      const mainPl = await loadDomain(domain);
      startProlog(mainPl);
      currentDomain = domain;
    }

    let output = "";
    const onData = data => {
      output += data.toString();
    };

    prolog.stdout.once("data", onData);

    prolog.stdin.write(query.trim().endsWith(".") ? query + "\n" : query + ".\n");

    setTimeout(() => {
      prolog.stdout.removeListener("data", onData);
      res.json({ result: output.trim() || "false" });
    }, 200);

  } catch (err) {
    console.error(err);
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`🔥 Prolog REPL server running on port ${port}`);
});
