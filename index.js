const express = require("express");
const cors = require("cors");
const { execFile } = require("child_process");
const path = require("path");

const app = express();
app.use(cors());
app.use(express.json());

app.post("/prolog", (req, res) => {
  const { query } = req.body;

  if (!query) return res.status(400).json({ error: "No query provided" });

  const prologFile = path.resolve(__dirname, "example1.pl");
  const hasVars = /[A-Z]/.test(query);
  const argsMatch = query.match(/\((.*)\)/);
  const args = argsMatch ? argsMatch[1] : "";

  const goal = hasVars
    ? `findall([${args}], ${query}, L), writeq(L), nl, halt.`
    : `${query}, write('true'), nl, halt.`;

  execFile("swipl", ["-q", "-s", prologFile, "-g", goal], (err, stdout, stderr) => {
    if (err) {
      res.status(500).json({ error: stderr || err.message });
    } else {
      res.json({ result: stdout.trim() || "false" });
    }
  });
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => console.log(`🧠 Prolog server running on port ${PORT}`));
