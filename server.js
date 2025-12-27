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

  return baseDir; // връщаме папката, не конкретен файл
}

// POST /prolog-run
// { domain: "animals", predicate: "select_file('bird.pl')" }
app.post("/prolog-run", async (req, res) => {
  const { domain, predicate } = req.body;
  if (!domain || !predicate) return res.status(400).json({ error: "Missing domain or predicate" });

  try {
    const baseDir = await loadDomain(domain);

    const tempFile = path.join(baseDir, "temp_run.pl");
    // Създаваме временен файл, който зарежда main.pl и стартира желания предикат
    const tempCode = `
:- [main].
:- initialization((${predicate}, halt)).
`;
    fs.writeFileSync(tempFile, tempCode, { encoding: "utf8" });

    execFile("swipl", ["-q", "-s", tempFile], (error, stdout, stderr) => {
      if (error) {
        res.json({ error: stderr || error.message });
      } else {
        res.json({ result: stdout.trim() || "true" });
      }
    });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.listen(port, () => {
  console.log(`Prolog server running on port ${port}`);
});
